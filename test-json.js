#!/usr/bin/env node

var test = function(name) {
	var content =
		require('fs').readFileSync(name);

	console.log(JSON.parse(content));
}

test('./lexicon.json')
test('./onomasticon.json')


