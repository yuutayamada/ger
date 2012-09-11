#!/usr/bin/ruby env
# -*- coding: utf-8 -*-

require 'google_reader_api'

module Ger
  class Api
    def initialize(account=false, password=false)
      @account  = account
      @password = password
      @user     = create_user()
      @marshal_file = File.join(Dir.home, ".gmail_feeds")
    end

    attr_accessor :account
    attr_accessor :password
    attr_accessor :user
    attr_accessor :marshal_file

    def create_user()
      begin
        user = nil
        if @account && @password
          user_api = GoogleReaderApi::Api.new({ email:    @account,
                                                password: @password })
          user = GoogleReaderApi::User.new({auth: user_api.auth})
        end
        @user = user
      rescue GoogleLogin::ClientLogin::BadAuthentication => e
        puts e.message
        puts "Mistaken account or password?"
      end
    end

    # @exmplle output feeds
    #   obj = Ger::Api.new("account@gmail.com", "your_password")
    #   obj.feeds.each do |factor|
    #     puts factor.title
    #     puts factor.url
    #   end
    def feeds()
      begin
        register()
        create_user() if @user == nil
        @user.feeds
      rescue NoMethodError => e
        puts e.message
      end
    end

    def register()
      unless @account && @password
        puts "Please, account and/or password"
        @account  = query("account") unless @account
        unless @password
          prefix = @account[0, 4]
          suffix = @account.match(/@.+$/).to_s
          @password = query("password for #{prefix}..#{suffix}")
        end
      end
      create_user()
    end

    def marshal(token_object)
      File.open @marshal_file, "wb" do |file|
        Marshal.dump token_object, file
      end
    end

    def demarshal()
      begin
        feeds = nil
        File.open @marshal_file, "rb" do |file|
          feeds = Marshal.load file
        end
        @user = feeds
      rescue Errno::ENOENT => e
        puts "marshal file: " + @marshal_file
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
