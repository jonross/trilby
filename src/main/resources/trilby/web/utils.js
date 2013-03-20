(function(){
    
    var row = function(start, end, cells) {
        var tr = $("<tr></tr>")
        cells.each(function(i, cell) {
            tr.append($(start + cell + end))
        })
        return tr
    }
    
    $.rowtd = function() { return row("<td>", "</td>", $(arguments)) }
    $.rowth = function() { return row("<th>", "</th>", $(arguments)) }
    
    var idSerial = 1
    
    $.uid = function() {
        return "id" + idSerial++
    }
    
})()