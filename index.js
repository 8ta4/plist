const plist = require("plist");

function convertPlistToJSON(xmlData) {
  const jsonData = plist.parse(xmlData);
  return JSON.stringify(jsonData, null, 2);
}

const xmlInput = process.argv[2];
console.log(convertPlistToJSON(xmlInput));
