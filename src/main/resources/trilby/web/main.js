
var ty = {

    classDefs: {
        
    },
    
    init: function() {
        ty.query("histo(x) from Object x")
    },

    query: function(s) {
        ty.request("query?q=" + s)
    },

    request: function(url) {
        $.ajax({
            url: url
        }).done(function(rsp) {
            (new ty[rsp.handler]).run(rsp.data)
            if (ty.pendingRequests.length > 0) {
                ty.pendingRequests.shift()()
            }
        }).fail(function(xhr, status) {
            alert("Internal error, response processing failed: " + status)
        })
        return {
            then: function(url) {
                ty.pendingRequests.push(url)
            }
        }
    },
    
    pendingRequests: []

}

$.Class("ty.Error", 
{},
{
    run: function(payload) {
        alert("Got an error: " + payload);
    }
})

$.Class("ty.InitUI",
{},
{
    run: function(payload) {
        // does nothing yet
    }
})

$.Class("ty.ClassDefs",
{},
{
    run: function(payload) {
        $(payload).each(function(i, classDef) {
            ty.classDefs[classDef.id] = classDef
        })
    }
})

$.Class("ty.Histo",
{},
{
    run: function(payload) {
        var table = $("<table></table>")
        $(payload).each(function(i, entry) {
            var classDef = ty.classDefs[entry.id]
            table.append($.row(classDef.name, entry.count, entry.nbytes))
        })
        $("body").append(table)
    }
})
