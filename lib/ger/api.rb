#!/usr/bin/ruby env
# -*- coding: utf-8 -*-

require 'google_reader_api'

module Ger
  class Api
    def initialize(account=false, password=false)
      @account  = account
      @password = password
      @user     = create_user()
    end

    attr_accessor :account
    attr_accessor :password

    def create_user()
      user = nil
      if @account && @password
        user_api = GoogleReaderApi::Api.new({ email:    @account,
                                              password: @password })
        user = GoogleReaderApi::User.new({auth: user_api.auth})
      end
      user
    end

    # @exmplle output feeds
    #   obj = Ger::Api.new("account@gmail.com", "your_password")
    #   obj.feeds.each do |factor|
    #     puts factor.title
    #     puts factor.url
    #   end
    def feeds()
      begin
        unless @account && @password
          puts "Please, account or password"
          @account  = query("account")  unless @account
          unless @password
            prefix = @account[0, 4]
            suffix = @account.match(/@.+$/).to_s
            @password = query("password for #{prefix}..#{suffix}")
          end
        end
        @user = create_user() if @user == nil
        @user.feeds
      rescue NoMethodError => e
        puts e.message
      end
    end

    def query(message)
      print "#{message}: "
      system "stty -echo"
      result = $stdin.gets.chop
      system "stty echo"
      result
    end
  end
end
