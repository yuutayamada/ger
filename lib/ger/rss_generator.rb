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

    def sort_json_by(item, record)
      json = JSON.parse(record)
      item_and_index = []
      json.to_ary.each_with_index do |subject, index|
        item_and_index << [subject[item], index]
      end
      sorted_item_and_index = item_and_index.sort_by {|item| item}.reverse # !> shadowing outer local variable - item
      sorted = []
      sorted_item_and_index.each_with_index do |item_and_index, this_index| # !> shadowing outer local variable - item_and_index
        content            = json.to_ary[item_and_index.last]
        sorted[this_index] = content
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
        stetement.sub!(/^\n ?/, "")
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
