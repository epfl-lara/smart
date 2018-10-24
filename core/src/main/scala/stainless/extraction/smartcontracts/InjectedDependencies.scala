package stainless
package extraction
package smartcontracts

object InjectedDependencies {

  /* ================================================
   *   Construction of Smart Contract Dependencies
   * ================================================ */

  import trees._

  val uint256 = BVType(false, 256)
  val uzero = BVLiteral(false, 0, 256)

  // Address
  val idField = ValDef.fresh("id", IntegerType())
  val addressCd = new ClassDef(
    ast.SymbolIdentifier("Address"),
    Seq(),
    Seq(),
    Seq(idField),
    Seq(Synthetic)
  )
  val addressType = ClassType(addressCd.id, Seq())

  // Msg
  val senderField = ValDef.fresh("sender", addressType)
  val valueField = ValDef.fresh("amount", uint256)
  val msgCd = new ClassDef(
    ast.SymbolIdentifier("Msg"),
    Seq(),
    Seq(),
    Seq(senderField, valueField),
    Seq(Synthetic)
  )
  val msgType = ClassType(msgCd.id, Seq())

  // Environment
  val balancesField = VarDef(
    FreshIdentifier("balances"),
    MapType(addressType, uint256),
    Seq()
  )
  val envCd = new ClassDef(
    ast.SymbolIdentifier("Environment"),
    Seq(),
    Seq(),
    Seq(balancesField),
    Seq(Synthetic, IsMutable)
  )
  val envType = ClassType(envCd.id, Seq())

  // Methods of Environment
  val fromTBParam = ValDef.fresh("from", addressType)
  val toTBParam = ValDef.fresh("to", addressType)
  val amountTBParam = ValDef.fresh("amount", uint256)
  val transferBalanceFd = new FunDef(
    ast.SymbolIdentifier("transferBalance"),
    Seq(),
    Seq(fromTBParam, toTBParam, amountTBParam),
    UnitType(),
    Block(
      Seq(
        FieldAssignment(This(envType), balancesField.id,
          MapUpdated(
            ClassSelector(This(envType), balancesField.id),
            fromTBParam.toVariable,
            Minus(
              MapApply(ClassSelector(This(envType), balancesField.id), fromTBParam.toVariable),
              amountTBParam.toVariable
            )
          )
        )
      ),
      FieldAssignment(This(envType), balancesField.id,
        MapUpdated(
          ClassSelector(This(envType), balancesField.id),
          toTBParam.toVariable,
          Plus(
            MapApply(ClassSelector(This(envType), balancesField.id), toTBParam.toVariable),
            amountTBParam.toVariable
          )
        )
      )
    ),
    Seq(Synthetic, IsMethodOf(envCd.id), Inline)
  )

  // Methods of Address
  val envBalanceParam = ValDef.fresh("env", envType)
  val balanceFd = new FunDef(
    ast.SymbolIdentifier("balance"),
    Seq(),
    Seq(envBalanceParam),
    uint256,
    MapApply(ClassSelector(envBalanceParam.toVariable, balancesField.id), This(addressType)),
    Seq(Synthetic, IsMethodOf(addressCd.id), Inline)
  )

  val amountTransferParam = ValDef.fresh("amount", uint256)
  val envTransferParam = ValDef.fresh("env", envType)
  val msgTransferParam = ValDef.fresh("msg", msgType)
  val transferFd = new FunDef(
    ast.SymbolIdentifier("transfer"),
    Seq(),
    Seq(amountTransferParam, envTransferParam, msgTransferParam),
    UnitType(),
    Assume(
      GreaterEquals(
        MapApply(ClassSelector(envTransferParam.toVariable, balancesField.id),
          ClassSelector(msgTransferParam.toVariable, senderField.id)),
        uzero
      ),
      MethodInvocation(envTransferParam.toVariable, transferBalanceFd.id, Seq(), Seq(
        ClassSelector(msgTransferParam.toVariable, senderField.id),
        This(addressType),
        amountTransferParam.toVariable
      ))
    ),
    Seq(Synthetic, IsMethodOf(addressCd.id), Inline)
  )

  val newClasses = Seq(addressCd, msgCd, envCd)
  val newFunctions = Seq(balanceFd, transferBalanceFd, transferFd)
}
