var mongoose = require('mongoose'),
    Schema = mongoose.Schema;

Document = new Schema({
  title: {type: String, index: true},
  data: String,
  tags: String
});

Document.virtual('id').get(function() {
  return this._id.toHexString();
});

mongoose.model('Document', Document);

exports.Document = function(db) {
  return db.model('Document');
};
