var mongoose = require('mongoose'),
    Schema = mongoose.Schema;

mongoose.model('Document', new Schema({
  title: {type: String, index: true},
  data: String,
  tags: String
}));

exports.Document = function(db) {
  return db.model('Document');
};
