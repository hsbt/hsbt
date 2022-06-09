if /^(?:latest|day|month|nyear|search)$/ =~ @mode then
	add_header_proc do
		google_analytics_insert_code
	end
end

def google_analytics_insert_code
	<<-HTML
    <script async src="https://www.googletagmanager.com/gtag/js?id=G-D0Q0VQY00J"></script>
    <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-D0Q0VQY00J');
    </script>
	HTML
end
