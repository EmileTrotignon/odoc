type html = Html_types.div Tyxml.Html.elt

val text_of_type : Odoc_model.Lang.TypeExpr.t -> string

val html_of_doc : Odoc_model.Comment.docs -> html
val text_of_doc : Odoc_model.Comment.docs -> string

val url : Odoc_model.Paths.Identifier.Any.t -> string