require "bundler/inline"

gemfile do
  source 'https://rubygems.org'
  gem "google-cloud-speech"
end

require "google/cloud/speech"
require 'pp'

speech = Google::Cloud::Speech.new(project: ARGV[0])
audio = speech.audio ARGV[1], encoding: :flac, language: "ja-JP", sample_rate: 16000)
operation = audio.process
operation.wait_until_done!
results = operation.results

pp results.map(&:transcript)
