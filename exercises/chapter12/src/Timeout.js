"use strict";

exports.setTimeoutImpl = function(millis, fn) {
    return function() {
        var t = setTimeout(function () { fn}, millis);
        return {};
    };
};
