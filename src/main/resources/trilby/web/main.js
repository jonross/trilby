//

var ty = {

    classDefs: {
        
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
    
    pendingRequests: [],
    
    addTab: function(name, content) {
        var id = $.uid(), div = $("<div></div>", { id: id }).append(content)
        $("#tabset").append(div).tabs('add', "#" + id, name)
    }

}

// Classes for response handling

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

$.Class("ty.DeprecatedHisto",
{},
{
    run: function(payload) {
        var thead = $("<thead/>").append($.rowth("Class", "Count", "Bytes"))
        var tbody = $("<tbody/>")
        var table = $("<table class='tytable'/>").append(thead).append(tbody)
        $(payload).each(function(i, entry) {
            var classDef = ty.classDefs[entry.id]
            var span = "<span>" + classDef.name + "</span>"
            span += "<span class='ui-icon ui-icon-triangle-1-e'></span>"
            span += "<span class='ui-icon ui-icon-arrowreturnthick-1-e'></span>"
            span += "<span class='ui-icon ui-icon-arrowreturnthick-1-n'></span>"
            tbody.append($.rowtd(span, entry.count, entry.nbytes))
        })
        ty.addTab("all classes", table)
    }
})

$.Class("ty.Histo",
{},
{
    run: function(payload) {
        var table = new ty.Table("all classes")
        $(payload).each(function(i, entry) {
            table.addClassDef(entry)
        })
    }
})

// Class for model/view

$.Class("ty.Table",
{},
{
    init: function(tabName) {
        var thead = $("<thead/>").append($.rowth("Class", "Count", "Bytes"))
        this.tbody = $("<tbody/>")
        this.table = $("<table class='tytable'/>").append(thead).append(this.tbody)
        ty.addTab(tabName, this.table)
    },

    addClassDef: function(stub) {
        var classDef = ty.classDefs[stub.id]
        var span = "<span>" + classDef.name + "</span>"
        span += "<span class='ui-icon ui-icon-triangle-1-e'></span>"
        span += "<span class='ui-icon ui-icon-arrowreturnthick-1-e'></span>"
        span += "<span class='ui-icon ui-icon-arrowreturnthick-1-n'></span>"
        this.tbody.append($.rowtd(span, stub.count, stub.nbytes))
    }
})

