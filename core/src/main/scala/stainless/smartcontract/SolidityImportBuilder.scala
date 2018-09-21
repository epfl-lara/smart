package stainless
package smartcontract

import extraction.xlang.{trees => xt}

object SolidityImportBuilder {
    def buildImports(filename: String)(implicit symbols: xt.Symbols): Seq[String] = {
        import xt._
        import exprOps._

        def formatImport(s: String) = {
            s.replace(".scala", ".sol")
        }

        def isIdentifier(name: String, id: Identifier) = id match {
            case ast.SymbolIdentifier(`name`) => true
            case _ => false
        }

        val classes = symbols.classes.values
        val functions = symbols.functions.values

        val typesToFile:Map[Type, String] = classes.map { cd =>
            cd.typed(symbols).toType -> cd.getPos.file.getCanonicalPath
        }.filterNot(_._2.contains("classes/stainless"))
        .toMap
        
        val funToFile:Map[Identifier, String] = functions.filter{ fd =>
            fd.flags.exists { case f => f match {
                case Annotation("solidityLibrary", _) => true
                case _ => false
            }}
        }.map { fd =>
            fd.id -> fd.getPos.file.getCanonicalPath
        }.filterNot(_._2.contains("classes/stainless"))
        .toMap

        val abstractMethods:Set[Identifier] = 
            classes.filter { cd =>
                cd.parents.exists{ case p => isIdentifier("stainless.smartcontracts.ContractInterface", p.id) }
            }.flatMap{ cd =>
                cd.methods(symbols)
            }.toSet

        def processFunction(fd: FunDef, checkBody:Boolean = true) = {
            val paramsTypes = fd.params.map(_.tpe)
            val body = withoutSpecs(fd.fullBody)

            if(!body.isEmpty && checkBody) {
                val calledFuns = collect[Identifier]{
                    case fi:FunctionInvocation => Set(fi.id)
                    case _ => Set()
                }(body.get)

                val bodyTypes = collect[Type]{
                    case Let(vd, _, _) => Set(vd.tpe)
                    case LetVar(vd, _, _) => Set(vd.tpe)
                    case _ => Set()
                }(body.get)

                ((paramsTypes ++ bodyTypes).map(t => typesToFile.getOrElse(t, "")) ++
                calledFuns.map(f => funToFile.getOrElse(f, "")))
            } else Set.empty
        }

        val imports1 = classes.filter { cd =>
            cd.getPos.file.getCanonicalPath == filename
        }.flatMap{ cd =>
            val parentsImports = cd.parents.map(t => typesToFile.getOrElse(t, ""))
            val fieldsImports = cd.fields.map(f => typesToFile.getOrElse(f.tpe, ""))

            (parentsImports ++ fieldsImports)
        }

        val imports2 = functions.filter { fd => 
            fd.getPos.file.getCanonicalPath == filename
        }.flatMap{ fd =>
            val isAbstract = fd.flags.collectFirst{ case IsMethodOf(id) => id }
                                     .flatMap(id => Some(abstractMethods.contains(id)))

            isAbstract match {
                case Some(b) => processFunction(fd, !b)
                case None => processFunction(fd)
            }
        }

        (imports1 ++ imports2)
            .toSet
            .filter(i => i != "" && i != filename)
            .map(formatImport)
            .toSeq
    }

}