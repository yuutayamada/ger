#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require 'open-uri'
require 'rss'
require 'json'
require 'fileutils'
require 'nokogiri'

class GoogleXmlParser
  def initialize()
    @record = []
  end

  attr_accessor :record

  def extract_from(xml)
    begin
      (xml/:outline).each_with_index do |elem, index|
        data = {}
        [:text, :type, :xmlUrl, :htmlUrl].each do |subject|
          data[subject] = elem[subject]
        end
        @record[index] = data
      end
    rescue
      $stderr.puts $!.inspect
    end
  end

  def parse(google_reader_xml_path)
    open(google_reader_xml_path) do |file|
      google_reader_xml =  file.read
      self.extract_from(Nokogiri::XML(google_reader_xml))
    end
  end
end

class RssGenerator
  @@test_url = 'http://www.nytimes.com/services/xml/rss/nyt/GlobalHome.xml'
  @@record ||= []
  @@file_name =  Dir.home + "/myrss.json"

  def initialize(sources = {test: @@test_url})
    @sources = sources
  end

  attr_accessor :sources

  def generate()
    sources = @sources.flatten
    sources.each_with_index do |url, index|
      make_rss_from(url) if url =~ /\.xml$/
    end
    @@record = JSON.pretty_generate(@@record)
  end

  def make_rss_from(url)
    if url
      rss_source = URI.parse(url)
      rss = nil
      begin
        rss = RSS::Parser.parse(rss_source)
        if defined? rss.items
          rss.items.each do |item|
            def item.description() "" end unless defined? item.description
            @@record << {
              title:       item.title,
              description: item.description.to_s.gsub(/<\/?[^>]*>/, ""),
              link:        item.link
            }
          end
        end
      rescue RSS::InvalidRSSError
        rss = RSS::Parser.parse(rss_source, false)
      end
    end
  end

  def output()
    print @@record
  end

  def methods()
    pretty_uri = URI.parse(@@test_url)
    rss = RSS::Parser.parse(pretty_uri.read)
    puts "SiteName : " + rss.channel.title
    p rss.items[0].methods
  end

  def backup(file_name)
    open(file_name) do |file|
      file.read
    end
    FileUtils.cp(file_name, file_name+"_bk")
    FileUtils.rm(file_name)
  end

  def reload()
    file_name = @@file_name
    backup(file_name) if File.exist?(file_name)
    self.generate()
    open(file_name, "w") do |fp|
      fp.write @@record
    end
  end
end

#parse xml of google reader
def get_xmls()
  google_reader_xml_path = Dir.home + "/Downloads/google-reader-subscriptions.xml"
  # memo: google_export = 'http://www.google.com/reader/subscriptions/export?hl=ja'
  parser = GoogleXmlParser.new
  parser.parse(google_reader_xml_path)
  parser.record
end

# @return list of title and xml
def create_xml_enum(xmls, xml_only=false)
  result = []
  xmls.each do |factor|
    something = xml_only ? factor[:xmlUrl] : [factor[:text], factor[:xmlUrl]]
    result << something
  end
  result
end

rss = RssGenerator.new(create_xml_enum(get_xmls, true))
rss.reload()

# JSON output
# rss.output()
