import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter
import javax.xml.bind.annotation.adapters.XmlAdapter

// abstract class StringOptionAdapter extends XmlAdapter[String, Option[String]] // this causes XmlAdapter to be cooked, 
// so that the bound `XmlAdapter` in `XmlJavaTypeAdapter`'s field  `value` with type:
//  `Class<? extends XmlAdapter>` becomes `XmlAdapter[_, _]`

// does not type check unless above definition is uncommented
case class Link(@XmlJavaTypeAdapter(classOf[XmlAdapter[String, Option[String]]]) val title: Option[String])