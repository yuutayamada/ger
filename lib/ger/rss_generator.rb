#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require 'open-uri'
require 'rss'
require 'json'
require 'fileutils'
require 'nokogiri'
require 'cgi'
require 'ger/api'

module Ger
  class RssGenerator
    @@test_url = 'http://www.nytimes.com/services/xml/rss/nyt/GlobalHome.xml'
    @@file_name =  ".ger.json"

    def initialize(sources = {test: @@test_url})
      @sources = sources.flatten
      @rss_path = File.join(Dir.home, @@file_name)
      @record = []
    end

    attr_accessor :sources
    attr_accessor :record
    attr_accessor :rss_path

    def save(directory=false, record=false)
      specified_dir = directory || Dir.home
      @rss_path = File.join(specified_dir, @@file_name)
      backup(@rss_path) if File.exist?(@rss_path)
      open(@rss_path, "w:UTF-8") do |fp|
        fp.write record
      end
      puts "save! #{@rss_path}"
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

    def formatter(array_data)
      response = []
      array_data.each do |hash|
        title = convert_string_from(hash[:title].to_s, :title).to_s
        desc  = convert_string_from(hash[:description].to_s, :summary).to_s
        link  = convert_string_from(hash[:link].to_s, :link).to_s
        date  = convert_string_from(hash[:date].to_s, :date).to_s
        response << {
          title:       title,
          description: desc,
          link:        link,
          date:        date,
          id:          hash[:id].to_s
        }
      end
      response
    end

    def convert_string_from(html, factor)
      content = CGI.unescapeHTML(html).to_s
      case factor
      when :title
        (Nokogiri::HTML(content)/factor).text
      when :summary
        portion = (Nokogiri::HTML(content)/factor).text.to_s
        # portion.gsub!(/ +/, " ")
        # portion.gsub!(/[\n\t]/, "")
      when :link
        (Nokogiri::HTML(content)/:link).attribute("href").value
      when :date
        (Nokogiri::HTML(content)/:published).children.text
      end
    end

    def extract_unread_items(user, verbose=false)
      begin
        user.feeds.each do |feed|
          feed.all_unread_items.each do |item|
            puts item if verbose
            item.methods
            store_feed(item.entry)
          end
        end

        unless @record.empty?
          @record = sort_json_by("date", to_json(formatter(@record)))
        else
          puts "Record empty!.... usage restrictions??"
        end
      rescue RuntimeError => e
        puts "RuntimeError: " + e.message
      rescue NoMethodError => e
        puts e.message
      end
    end

    def to_json(data)
      JSON.pretty_generate(data)
    end

    def backup(file_name)
      open(file_name) do |file|
        file.read
      end
      FileUtils.cp(file_name, file_name+"_bk")
      FileUtils.rm(file_name)
    end

    def store_feed(html)
        @record << {
          title:       html.title,
          description: html.summary,
          link:        html.link,
          date:        html.published,
          id:          html.id
        }
    end

  end
end
