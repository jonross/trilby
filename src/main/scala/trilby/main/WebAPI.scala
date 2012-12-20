package trilby.main
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import trilby.hprof.Heap
import trilby.util.Oddments._
import org.eclipse.jetty.server.Request
import org.codehaus.jackson.JsonFactory
import java.io.StringWriter
import com.google.common.io.ByteStreams
import trilby.reports.FullHistogram
import org.codehaus.jackson.JsonGenerator

class WebAPI(heap: Heap, baseReq: Request,
             req: HttpServletRequest, rsp: HttpServletResponse) {
    
    implicit def jsonifier(m: Map[String,Any]) = new { def toJSON = jsonify(m) }
    implicit def jsonifier(s: Seq[Any]) = new { def toJSON = jsonify(s) }
    
    def handle() {
        val path = req.getPathInfo replaceAll ("https?://", "")
        rsp setStatus HttpServletResponse.SC_OK
        path match {
            case s if s endsWith ".js" => sendFile(s, "text/javascript")
            case s if s endsWith ".css" => sendFile(s, "text/css")
            case s if s endsWith ".html" => sendFile(s, "text/html")
            case s if s endsWith "/init" => sendJson(initUI)
            case s if s endsWith "/classdefs" => sendJson(classDefs)
            case s if s endsWith "/fullhisto" => sendJson(fullHisto)
            case s => sendJson(oops("No response for " + path))
        }
        baseReq setHandled true
    }
    
    private def sendFile(path: String, mimeType: String) {
        println ("serve " + path)
        using (getClass getResourceAsStream ("../web" + path)) { in =>
            rsp setContentType mimeType + ";charset=utf-8"
            ByteStreams copy (in, rsp getOutputStream)
        }
    }
    
    private def sendJson(json: String) {
        rsp setContentType "application/json;charset=utf-8"
        rsp.getOutputStream print json
    }
    
    private def initUI = Map(
        "handler" -> "InitUI",
        "data" -> List()
    ).toJSON
    
    private def classDefs = Map(
        "handler" -> "ClassDefs",
        "data" -> (for (c <- heap.classes) yield Map(
            ("id" -> c.classId),
            ("name" -> c.name)
         ))
    ).toJSON
    
    private def fullHisto = Map(
        "handler" -> "Histo",
        "data" -> (for (entry <- new FullHistogram(heap)) yield Map(
            "id" -> entry.classDef.classId,
            "count" -> entry.count,
            "nbytes" -> entry.nbytes
        ))
    ).toJSON

    private def oops(msg: String) = Map(
        "handler" -> "Error",
        "data" -> msg
    ).toJSON
    
    private val jsonFactory = new JsonFactory()
        
    private def jsonify(x: AnyRef): String = {
        val out = new StringWriter()
        val gen = jsonFactory.createJsonGenerator(out)
        jsonify(gen, x)
        gen.flush()
        out.toString
    }
    
    private def jsonify(gen: JsonGenerator, x: Any): Unit = x match {
        case m: Map[String, Any] =>
            gen.writeStartObject()
            for ((k, v) <- m) {
                gen.writeFieldName(k)
                jsonify(gen, v)
            }
            gen.writeEndObject()
        case s: Seq[Any] =>
            gen.writeStartArray()
            for (i <- s)
                jsonify(gen, i)
            gen.writeEndArray()
        case n: Int =>
            gen.writeNumber(n)
        case f: Float =>
            gen.writeNumber(f)
        case _ =>
            gen.writeString(x.toString)
    }
}
