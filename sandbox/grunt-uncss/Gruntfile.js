module.exports = function (grunt) {
  'use strict';
  grunt.initConfig({
    pkg: grunt.file.readJSON("package.json"),
    uncss: {
      dist: {
        files: {
          'public/stylesheets/tidy.css': ['public/404.html']
        }
      }
    }
  });
  grunt.loadNpmTasks('grunt-uncss');
  grunt.registerTask('default', ['uncss:dist']);
};
