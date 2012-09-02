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
    end

    attr_accessor :sources

    def generate()
        make_rss_from(url) if url =~ /\.xml$/
        @sources.each_with_index do |url, index|
      end
      @@record = JSON.pretty_generate(@@record)
      @@record = sort_json(@@record)
    end

    def sort_json(record)
      json = JSON.parse(record)
      time_and_index = []
      json.to_ary.each_with_index do |h, index|
        time_and_index << [h["date"], index]
      end
      sort_index = time_and_index.sort_by {|a| a }
      sorted = []
      sort_index.reverse.each_with_index do |time_and_index, this_index|
        sorted[this_index] = json.to_ary[time_and_index[1]]
      end
      JSON.pretty_generate(sorted)
    end

    def to_time(text)
      require 'date/format'
      require 'time'
      ten_days_ago = Time.now.-(60*60*24*10)
      begin
        if text == ""
          Time.now
        else
          array = Date._parse(text, false).values_at(:year, :mon, :mday,
                                                     :hour, :min, :sec,
                                                     :zone, :wday)
          Time.mktime(*array)
        end
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
      if url
        rss_source = URI.parse(url)
        rss = nil

        begin
          rss = RSS::Parser.parse(rss_source)
          if defined? rss.items
            rss.items.each_with_index do |item, index|
              def item.description() "" end unless defined? item.description
              subset = {
                title:       self.format(item.title.to_s),
                description: self.format(item.description.to_s),
                link:        self.format(item.link.to_s, true),
                date:        to_time(item.date.to_s)
              }
              if n_days_ago(2) < subset[:date]
                @@record.unshift subset
              end
            end
          end
        rescue RSS::InvalidRSSError
          rss = RSS::Parser.parse(rss_source, false)
        end
      end
    end

    def format(description, link=false)
      if link
        description = description.match(/http.+/).to_s.chomp('"')
      else
        description.gsub!(/&nbsp;/, " ")
        description.gsub!(/&amp;/, "&")
        description.gsub!(/<\/?[^>]*>|\n\n+/, "")
        description.gsub!(/  +/, " ")
      end
      description
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

    def reload(directory)
      specific_dir = directory || Dir.home
      file_name = specific_dir + "/" + @@file_name
      backup(file_name) if File.exist?(file_name)
      self.generate()
      open(file_name, "w") do |fp|
        fp.write @@record
      end
    end
  end
end
