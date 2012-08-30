# -*- coding: utf-8 -*-
#
# Copyright (C) 2012 Yuta Yamada
#
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

require 'thor'
require 'ger/xml_parser'
require 'ger/rss_generator'

module Ger
  class Command < Thor

    @@rss

    def initialize(*args)
      super
      @parser                 = Ger::XmlParser.new
      @generator              = Ger::RssGenerator.new
      # memo: google_export = 'http://www.google.com/reader/subscriptions/export?hl=ja'
      @google_reader_xml_path = Dir.home + "/Downloads/google-reader-subscriptions.xml"
    end

    desc 'reload', 'Update google-reader-subscriptions.xml'
    method_option "directory", type: :string, default: false, aliases: "--dict"
    def reload()
      init(options["directory"])
      @@rss.reload()
    end

    desc 'status', 'show google_reader_xml_path'
    def status()
      puts @google_reader_xml_path
    end

    private

    def init(option=false)
      if option && option["directory"]
        @google_reader_xml_path = opt["directory"]
      end
      @@rss = RssGenerator.new(create_xml_enum(get_xmls, true))
    end

    # parse xml of google reader
    def get_xmls()
      @parser.parse(@google_reader_xml_path)
      @parser.record
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

    # TODO: create alias reload or update
    # TODO: google reader database update
  end
end
