#!/usr/bin/ruby env
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

# require 'thor'
require 'open-uri'
require 'rss'
require 'json'
require 'fileutils'
require 'nokogiri'

module Ger
  class XmlParser

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
end
