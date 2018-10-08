package stainless
package solidity

trait SolidityDef

case class SContractDefinition(
    name: String,
    parents: Seq[String],
    constructor: SConstructorDef,
    events: Seq[SEventDef],
    enums: Seq[SEnumDefinition],
    fields: Seq[SParamDef],
    methods: Seq[SFunDef]
) extends SolidityDef

case class SContractInterface(
    name: String,
    events: Seq[SEventDef],
    methods: Seq[SAbstractFunDef]
) extends SolidityDef

case class SLibrary(
    name: String,
    functions: Seq[SFunDef]
) extends SolidityDef

case class SolidityImport(path: String)

case class SParamDef(constant: Boolean, name: String, tpe: SolidityType) extends SolidityDef
case class SEnumDefinition(name: String, values: Seq[String]) extends SolidityDef
case class SEventDef(name: String, args: Seq[SParamDef]) extends SolidityDef
case class SConstructorDef(params: Seq[SParamDef], body: SolidityExpr) extends SolidityDef
case class SFunDef(name: String,params: Seq[SParamDef], returnType: SolidityType, body: SolidityExpr, flags: Seq[SFlag]) extends SolidityDef
case class SAbstractFunDef(name: String, params: Seq[SParamDef], returnType: SolidityType, flags: Seq[SFlag]) extends SolidityDef

sealed trait SolidityExpr

//case class SParamDef(name: String, tpe: SolidityType) extends SolidityExpr
case class SEnumValue(id: String, value: String) extends SolidityExpr
case class SVariable(name: String) extends SolidityExpr
case class SMethodInvocation(rcv: SolidityExpr, method: String, args: Seq[SolidityExpr], ether: Option[SolidityExpr]) extends SolidityExpr
case class SFunInvocation(fun: String, args: Seq[SolidityExpr]) extends SolidityExpr
case class SThis() extends SolidityExpr
case class SSuper() extends SolidityExpr
case class SEvent(tpe: SEventType, args: Seq[SolidityExpr]) extends SolidityExpr
case class SClassConstructor(tpe: SolidityType, args: Seq[SolidityExpr]) extends SolidityExpr
case class SClassSelector(expr: SolidityExpr, id: String) extends SolidityExpr
case class SLiteral(value: String) extends SolidityExpr
case class STerminal() extends SolidityExpr
case class SLet(vd: SParamDef, value: SolidityExpr, body: SolidityExpr) extends SolidityExpr
case class SNow() extends SolidityExpr
case class SAddress(code: SolidityExpr) extends SolidityExpr
case class SSelfDestruct(receiver: SolidityExpr) extends SolidityExpr
case class STransfer(receiver: SolidityExpr, amount: SolidityExpr) extends SolidityExpr
case class SAssignment(rcv: SolidityExpr, value: SolidityExpr) extends SolidityExpr
case class SEmit(event: SClassConstructor) extends SolidityExpr
case class SWhile(cond: SolidityExpr, body: SolidityExpr) extends SolidityExpr

case class SMappingRef(rcv: SolidityExpr, index: SolidityExpr) extends SolidityExpr
case class SArrayRef(rcv: SolidityExpr, index: SolidityExpr) extends SolidityExpr
case class SArrayLength(rcv: SolidityExpr) extends SolidityExpr

case class SFieldAssignment(obj: SolidityExpr, selector: String, expr: SolidityExpr) extends SolidityExpr
case class SFunctionInvocation(name: String, args: Seq[SolidityExpr]) extends SolidityExpr
case class SIfExpr(cond: SolidityExpr, thenn: SolidityExpr, elze: SolidityExpr) extends SolidityExpr
case class SBlock(exprs: Seq[SolidityExpr], last: SolidityExpr) extends SolidityExpr

case class SReturn(expr: SolidityExpr) extends SolidityExpr
case class SAnd(exprs: Seq[SolidityExpr]) extends SolidityExpr
case class SOr(exprs: Seq[SolidityExpr]) extends SolidityExpr
case class SNot(expr: SolidityExpr) extends SolidityExpr

case class SEquals(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr
case class SGreaterThan(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr
case class SGreaterEquals(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr
case class SLessThan(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr
case class SLessEquals(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr

case class SPlus(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr
case class SMinus(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr
case class SDivision(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr
case class SMult(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr
case class SRemainder(left: SolidityExpr, right: SolidityExpr) extends SolidityExpr

case class SRequire(cond: SolidityExpr, errorMessage: String) extends SolidityExpr
case class SAssert(cond: SolidityExpr, errorMessage: String) extends SolidityExpr


sealed trait SolidityType
case class SUIntType(size: Int) extends SolidityType
case class SIntType(size: Int) extends SolidityType
case class SAddressType() extends SolidityType
case class SBooleanType() extends SolidityType
case class SStringType() extends SolidityType
// case class SInt32Type() extends SolidityType
case class SUnitType() extends SolidityType
case class SEnumType(id: String) extends SolidityType
case class SContractType(id: String) extends SolidityType
case class SMapping(key: SolidityType, vvalue: SolidityType) extends SolidityType
case class SEventType(id: String) extends SolidityType
case class SArrayType(underlying: SolidityType) extends SolidityType

sealed trait SFlag
case class SPayable()                  extends SFlag
case class SPure()                     extends SFlag
case class SView()                     extends SFlag
case class SPrivate()                  extends SFlag

object SolidityTreeOps {
    def transform(f: PartialFunction[SolidityExpr, SolidityExpr], expr: SolidityExpr) = {
        def process(expr: SolidityExpr): SolidityExpr = expr match {
            case e if f.isDefinedAt(e) => f(e)
            case SMethodInvocation(rcv, method, args, ether) => SMethodInvocation(process(rcv), method, args.map(process), ether.map(process))
            case SClassConstructor(tpe, args) => SClassConstructor(tpe, args.map(process))
            case SClassSelector(expr, sel) => SClassSelector(process(expr), sel)
            case SLet(vd, value, body) => SLet(vd, process(value), process(body))
            case SAddress(value) => SAddress(process(value))
            case SArrayRef(rcv, index) => SArrayRef(process(rcv),process(index))
            case SAssignment(rcv, value) => SAssignment(process(rcv), process(value))
            case SWhile(cond, body) => SWhile(process(cond), process(body))
            case SFieldAssignment(obj, sel, expr) => SFieldAssignment(process(obj), sel, process(expr))
            case SFunctionInvocation(name, args) => SFunctionInvocation(name, args.map(process))
            case SIfExpr(cond, thenn, elze) => SIfExpr(process(cond), process(thenn), process(elze))
            case SBlock(exprs, last) => SBlock(exprs.map(process), process(last))
            case SReturn(expr) => SReturn(process(expr))
            
            case SAnd(exprs) => SAnd(exprs.map(process))
            case SOr(exprs) => SOr(exprs.map(process))
            case SNot(expr) => SNot(process(expr))

            case SEquals(left, right) => SEquals(process(left), process(right))
            case SGreaterThan(left, right) => SGreaterThan(process(left), process(right))
            case SGreaterEquals(left, right) => SGreaterEquals(process(left), process(right))
            case SLessThan(left, right) => SLessThan(process(left), process(right))
            case SLessEquals(left, right) => SLessEquals(process(left), process(right))

            case SPlus(left, right) => SPlus(process(left), process(right))
            case SMinus(left, right) => SMinus(process(left), process(right))
            case SDivision(left, right) => SDivision(process(left), process(right))
            case SMult(left, right) => SMult(process(left), process(right))

            case e => e
        }

        process(expr)
    }
}