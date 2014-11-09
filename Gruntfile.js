module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "bower_components/**/src/**/*.purs",
      "src/**/*.purs"
    ],
    
    clean: ["tmp", "output"],
  
    psc: {
      options: {
        modules: ["Main"]
      },
      lib: {
        src: ["<%=libFiles%>"],
        dest: "js/Main.js"
      }
    },

    dotPsci: ["<%=libFiles%>"],
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
 
  grunt.registerTask("make", ["psc:lib", "dotPsci"]);
  grunt.registerTask("default", ["clean", "make"]);
};
