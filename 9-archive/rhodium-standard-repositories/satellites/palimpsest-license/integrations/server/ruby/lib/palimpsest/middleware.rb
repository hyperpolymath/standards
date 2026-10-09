# frozen_string_literal: true

module Palimpsest
  # Rack middleware for Palimpsest License integration
  #
  # This middleware automatically injects Palimpsest License metadata
  # into HTTP responses.
  #
  # @example Basic usage
  #   use Palimpsest::Middleware, {
  #     work_title: 'My Work',
  #     author_name: 'Jane Doe'
  #   }
  class Middleware
    # Initialize middleware
    #
    # @param app [Object] Rack application
    # @param options [Hash] Configuration options
    def initialize(app, options = {})
      @app = app
      @config = Config.new(options)
      @generator = Generator.new(@config)
    end

    # Process request
    #
    # @param env [Hash] Rack environment
    # @return [Array] Rack response
    def call(env)
      status, headers, response = @app.call(env)

      # Inject HTTP headers
      inject_http_headers(headers) if @config.inject_headers

      # Inject HTML metadata for HTML responses
      if html_response?(headers)
        response = inject_html_metadata(response)
      end

      [status, headers, response]
    end

    private

    # Check if response is HTML
    def html_response?(headers)
      content_type = headers['Content-Type'] || headers['content-type']
      content_type&.include?('text/html')
    end

    # Inject HTTP headers
    def inject_http_headers(headers)
      @generator.http_headers.each do |key, value|
        headers[key] = value
      end
    end

    # Inject HTML metadata
    def inject_html_metadata(response)
      body = ''
      response.each { |part| body << part }

      # Inject meta tags
      if @config.inject_html_meta && body.include?('</head>')
        meta_tags = @generator.generate_html_meta
        body = body.sub('</head>', "    #{meta_tags}\n  </head>")
      end

      # Inject JSON-LD
      if @config.inject_jsonld && body.include?('</body>')
        jsonld = @generator.generate_jsonld
        script_tag = "\n    <script type=\"application/ld+json\">\n#{jsonld}\n    </script>"
        body = body.sub('</body>', "#{script_tag}\n  </body>")
      end

      [body]
    end
  end
end
