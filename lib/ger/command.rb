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
require 'ger/rss_generator'
require 'ger/api'

module Ger
  class Command < Thor
    def initialize(*args)
      super
      @google_reader = Ger::Api.new()
    end

    desc 'reload', 'Update google-reader-subscriptions.xml'
    method_option "directory", type: :string, default: false, aliases: "-d"
    method_option "account", type: :string, default: false
    def reload()
      @google_reader.account  = options["account"]
      rss = Ger::RssGenerator.new(extract_google_reader_xmls())
      rss.reload(options["directory"])
    end

    private

    def extract_google_reader_xmls()
      xmls = []
      @google_reader.feeds.each do |feed|
        url = feed.url
        xmls << url if url =~ /\.xml$/
      end
      xmls
    end
  end
end
