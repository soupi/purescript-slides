exports.setHtml = function(html) {
    return function() {
        document.getElementById("main").outerHTML = html;
    }
};
