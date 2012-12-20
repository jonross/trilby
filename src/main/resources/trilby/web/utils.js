$.row = function() {
    var tr = $("<tr></tr>")
    $(arguments).each(function(i, cell) {
        tr.append($("<td>" + cell + "</td>"))
    })
    return tr
}
