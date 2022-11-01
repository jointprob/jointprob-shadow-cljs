module.exports = {
  plugins: [
    require('postcss-import'),
    require('autoprefixer'),
    require('@fullhuman/postcss-purgecss') ( {
        content: ['public/js/compiled/bayes.js', 'public/js/compiled/memory-efficiency.js', 'public/js/compiled/memory-efficiency.js']
    }),
    require('cssnano')
  ]}