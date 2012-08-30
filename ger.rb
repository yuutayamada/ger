#!/usr/bin/ruby
# -*- coding: utf-8 -*-

# Author: Yuta Yamada <cokesboy"at"gmail.com>
# URL: https://github.com/yuutayamada/ger
# Version: 0.0.1
# Keywords: toy

# License:
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

  def to_time(text)
    require 'date/format'
    require 'time'
    ten_days_ago = Time.now.-(60*60*24*10)
    begin
      if text == ""
        Time.now
      else
        require 'date/format'
        require 'time'

        array = Date._parse(text, false).values_at(:year, :mon, :mday,
                                                   :hour, :min, :sec,
                                                   :zone, :wday)
        time = Time.mktime(*array)
        time
      end
    rescue NoMethodError
      ten_days_ago
    rescue TypeError
      ten_days_ago
    end
  end

  def make_rss_from(url)
    if url
      rss_source = URI.parse(url)
      rss = nil
      begin
        rss = RSS::Parser.parse(rss_source)
        if defined? rss.items
          rss.items.each_with_index do |item, index|
            def item.description() "" end unless defined? item.description
            subset = {
              title:       item.title,
              description: item.description.to_s.gsub(/<\/?[^>]*>| +|&nbsp;|\n\n+/, ""),
              link:        item.link,
              date:        to_time(item.date.to_s)
            }
            if Time.now.-(60*60*24*2) < subset[:date]
              @@record << subset
            else
              @@record.unshift subset
            end
          end
        end
      rescue RSS::InvalidRSSError
        rss = RSS::Parser.parse(rss_source, false)
      end
    end
  end

  def output()
    if 0 == @@record.length
      self.generate()
    end
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
