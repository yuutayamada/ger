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

    @@record

    def initialize(*args)
      super
      @google_reader = Ger::Api.new()
      @user          = @google_reader.demarshal()
      @rss           = Ger::RssGenerator.new()
    end

    desc 'fetch', 'Fetch feeds from google reader of your account'
    method_option "directory", type: :string,  default: false, aliases: "-d"
    method_option "account",   type: :string,  default: false
    method_option "verbose",   type: :boolean, default: false
    def fetch(command=false)
      case command
      when "unread"
        save_unread_items(options["verbose"])
      end
    end

    desc 'register', 'Register feeds from google reader of your account'
    def register()
      @google_reader.register()
      @google_reader.marshal(@google_reader.user)
    end

    desc 'read_items', 'wip'
    def read_items
      @@record = []
      @user.feeds.each do |feed|
        @@record << feed.read_items
      end
      puts @@record
    end

    desc 'remove',
    'remove [ID] : remove feed that was already read of id, must be
     specify the id to argument of first'
    def remove(id=false)
      if id
        @google_reader.remove_feed_of_id(@user, id.to_s)
      else
        puts "Require ID : $ ger remove [Feed ID]"
      end
    end

    desc 'feeds', 'display feeds list'
    def feeds
      begin
        puts @user.feeds
      rescue SocketError => e
        puts "SocketError" + e.message
      end
    end

    desc 'id_exist?', 'for test'
    def id_exist(id=false)
      @google_reader.id_exist?(id) if id
    end

    private

    def save_unread_items(verbose=false)
      directory = options["directory"] ? options["directory"] : false
      @rss.extract_unread_items(@user, verbose)
      @rss.save(directory, @rss.record)
    end
  end
end
