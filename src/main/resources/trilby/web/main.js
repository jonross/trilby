//

var ty = {

    classDefs: null,

    // Simple shortcut around ty.request
    
    query: function(s) {
        ty.request("query?q=" + s)
    },

    // Generic request / response handling.  Responses should be
    //     { handler: 'name', data: ... }
    // we will run a function from ty.ResponseHandler with the data.
    //
    // The return value allows chaining of requests, each activating
    // when the prior completes, e.g.
    //     ty.request("/a").then("/b").then("/c")

    request: function(url) {
        $.ajax({
            url: url
        }).done(function(rsp) {
            (new ty.ResponseHandler)["do"+rsp.handler](rsp.data)
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

    // Adds a main window tab
    //   name: tab label
    //   content: child element
    
    addTab: function(name, content) {
        var id = $.uid(), div = $("<div></div>", { id: id }).append(content)
        $("#tabset").append(div).tabs('add', "#" + id, name)
    }

}

$.Class("ty.ResponseHandler", 
{},
{
    doError: function(payload) {
        alert("Got an error: " + payload);
    },

    doInitUI: function(payload) {
        ty.classDefs = new ty.ClassDefs()
    },

    doClassDefs: function(payload) {
        $(payload).each(function(i, def) {
            ty.classDefs.addClassDef(def)
        })
    },

    doHisto: function(payload) {
        var histo = new ty.TreeHisto(true)
        $(payload).each(function(i, entry) {
            // table.addClassDef(entry)
            histo.add(ty.classDefs.get(entry.id), entry.count, entry.nbytes)
        })
        histo.render()
    }
})

// Class for managing heap classdefs

$.Class("ty.ClassDefs",
{},
{
    byId: {},

    addClassDef: function(def) {
        this.byId[def.id] = def
    },

    get: function(id) {
        return this.byId[id]
    }
    
})

// Model object for a tree histogram

$.Class("ty.TreeHisto",
{},
{
    init: function(isRoot) {
        this.isRoot = isRoot
        this.count = this.nBytes = 0
        this.kids = {}
    },
    
    // Add a classdef with object & byte counts to the tree.  Splits the class name
    // and adds the counts to each labeled child node.

    add: function(classDef, count, nBytes) {
        this.count += count
        this.nBytes += nBytes
        var node = this
        $(classDef.name.split(".")).each(function(i, part) {
            var subnode = node.kids[part]
            if (subnode == null) 
                subnode = node.kids[part] = new ty.TreeHisto(false)
            subnode.count += count
            subnode.nBytes += nBytes
            node = subnode
        })
    },

    //
    
    render: function() {
        console.log(this)
        var thead = $("<thead/>").append($.rowth("Class / Package", "Count", "Bytes"))
        this.tbody = $("<tbody/>")
        this.table = $("<table class='tytable'/>").append(thead).append(this.tbody)
        ty.addTab("all classes", this.table)
        
        for (part in this.kids) {
            var subnode = this.kids[part]
            var span = "<span>" + part + "</span>"
            span += "<span class='ui-icon ui-icon-triangle-1-e'></span>"
            span += "<span class='ui-icon ui-icon-arrowreturnthick-1-e'></span>"
            span += "<span class='ui-icon ui-icon-arrowreturnthick-1-n'></span>"
            this.tbody.append($.rowtd(span, subnode.count, subnode.nBytes))
        }
    }

    // Based on hierarchical class names, generate the package/class
    // tree where each node is either a ClassInfo or PackageInfo
})