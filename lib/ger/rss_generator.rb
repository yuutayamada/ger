#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require 'open-uri'
require 'rss'
require 'json'
require 'fileutils'

module Ger
  class RssGenerator
    @@test_url = 'http://www.nytimes.com/services/xml/rss/nyt/GlobalHome.xml'
    @@record ||= []
    @@file_name =  ".ger.json"

    def initialize(sources = {test: @@test_url})
      @sources = sources.flatten
      @rss_path
    end

    attr_accessor :sources

    def generate()
      begin
        current_url_and_index = ""
        @sources.each_with_index do |url, index|
          make_rss_from(url)
        end
        @@record = JSON.pretty_generate(@@record)
        @@record = sort_json_by("date", @@record)
        puts "Done! spawned Rss to #{@rss_path}"
      rescue Encoding::UndefinedConversionError => e
        puts e.message + current_url_and_index
      end
    end

    def reload(directory)
      specified_dir = directory || Dir.home
      @rss_path = specified_dir.chomp("/") + "/" + @@file_name
      backup(@rss_path) if File.exist?(@rss_path)
      self.generate()
      open(@rss_path, "w:UTF-8") do |fp|
        fp.write @@record
      end
    end

    def sort_json_by(item, record)
      json = JSON.parse(record)
      item_and_index = []
      json.to_ary.each_with_index do |subject, index|
        item_and_index << [subject[item], index]
      end
      sorted_item_and_index = item_and_index.sort_by {|item| item}.reverse
      sorted = []
      sorted_item_and_index.each_with_index do |item_and_index, this_index|
        content            = json.to_ary[item_and_index.last]
        sorted[this_index] = content
      end
      JSON.pretty_generate(sorted)
    end

    def to_time(text)
      require 'date/format'
      require 'time'
      begin
        array = Date._parse(text, false).values_at(:year, :mon, :mday,
                                                   :hour, :min, :sec,
                                                   :zone, :wday)
        Time.mktime(*array)
      rescue NoMethodError
        n_days_ago(10)
      rescue TypeError
        n_days_ago(10)
      end
    end

    def n_days_ago(n)
      Time.now.-(60 * 60 * 24 * n.to_i)
    end

    def make_rss_from(url)
      begin
        source = URI.parse(url)
        rss = RSS::Parser.parse(source)
        rss.items.each do |item|
          def item.description() "" end unless defined? item.description
          subset = {
            title:       self.format(item.title.to_s),
            description: self.format(item.description.to_s),
            link:        self.format(item.link.to_s, true),
            date:        to_time(item.date.to_s),
          }
          if n_days_ago(2) < subset[:date]
            @@record << subset
          end
        end
      rescue NoMethodError => e
        puts e.message + "\n  URL: " + url
      rescue RSS::InvalidRSSError => e
        puts e.message + "\n  URL: " + url
        rss = RSS::Parser.parse(source, false)
      rescue Errno::EHOSTUNREACH => e
        puts e.message + "\n  URL: " + url
      end
    end

    def format(stetement, link=false)
      if link
        stetement = stetement.match(/http.+/).to_s.chomp('"')
      else
        stetement.gsub!(/\"/, "")
        stetement.gsub!(/&nbsp;/, " ")
        stetement.gsub!(/&amp;/, "&")
        stetement.gsub!(/<\/?[^>]*>|\n\n+/, "")
        stetement.gsub!(/  +/, " ")
        stetement.gsub!(/[\n\t] ?/, "")
      end
      stetement
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
  end
end
