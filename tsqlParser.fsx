#r "nuget: Microsoft.SqlServer.TransactSql.ScriptDom, 15.0.4200.1"
let parseQuery tsql =
    let parser = Microsoft.SqlServer.TransactSql.ScriptDom.TSql140Parser( true)
    let tsqlReader = new System.IO.StringReader(tsql)
    let errors = ref Unchecked.defaultof<_>
    let fragment = parser.Parse(tsqlReader, errors)
    let allVars = ResizeArray()
    let declaredVars = ResizeArray()
    let mutable target = ""
    let mutable table = ""
    fragment.Accept {
        new Microsoft.SqlServer.TransactSql.ScriptDom.TSqlFragmentVisitor() with
            member __.Visit(node : Microsoft.SqlServer.TransactSql.ScriptDom.VariableReference) = 
                base.Visit node
                //printf "%s" node.
                allVars.Add(node.Name, node.StartOffset, node.FragmentLength)
            member __.Visit(node : Microsoft.SqlServer.TransactSql.ScriptDom.DeclareVariableElement) = 
                base.Visit node
                declaredVars.Add(node.VariableName.Value)
            member __.Visit(node : Microsoft.SqlServer.TransactSql.ScriptDom.UpdateStatement) = 
                base.Visit node
                node.UpdateSpecification.Target.Accept {
                    new Microsoft.SqlServer.TransactSql.ScriptDom.TSqlFragmentVisitor() with
                        member __.Visit(node : Microsoft.SqlServer.TransactSql.ScriptDom.SchemaObjectName) = 
                            base.Visit node
                            target <- node.BaseIdentifier.Value
                }
                node.Accept{
                    new Microsoft.SqlServer.TransactSql.ScriptDom.TSqlFragmentVisitor() with
                        member __.Visit(node : Microsoft.SqlServer.TransactSql.ScriptDom.FromClause) = 
                            base.Visit node
                            node.Accept{
                                new Microsoft.SqlServer.TransactSql.ScriptDom.TSqlFragmentVisitor() with
                                    member __.Visit(node : Microsoft.SqlServer.TransactSql.ScriptDom.NamedTableReference) = 
                                        base.Visit node
                                        if node.Alias.Value = target then
                                            table <- node.SchemaObject.BaseIdentifier.Value
                            }
                }
    }
    let unboundVars = 
        allVars 
        |> Seq.groupBy (fun (name, _, _)  -> name)
        |> Seq.choose (fun (name, xs) -> 
            if declaredVars.Contains name 
            then None 
            else Some(name, xs |> Seq.mapi (fun i (_, start, length) -> sprintf "%s%i" name i, start, length)) 
        )
        |> dict

    table, target

let query = "select * from person where age = @hello and name=@hello"
let query1 = @"UPDATE dm_itemlocation
SET dm_Cost =
		-- recipes received from a commissary should not update item location prices based on ingredient cost
		-- approved prep logs are responsible for updating prepped recipe unit costs
		CASE
			WHEN ISNULL(il.ReceivedFromCommissary, 0) <> 1 AND ISNULL(il.PreppedItem, 0) <> 1
				THEN recipeCost.UnitCost * recipeCostUofMToInvUofM.costMultiplier
			ELSE il.dm_Cost
		END,
	CurrentBatchCost = recipeCost.UnitCost * recipeCostUofMToYieldUofM.costMultiplier * i.dm_YieldQty
FROM dbo.dm_itemlocation il
	JOIN dm_item i ON il.dm_item = i.dm_itemId
	JOIN #ItemLocationCosts recipeCost on il.dm_item = recipeCost.ItemId
			AND il.dm_Location = recipeCost.LocationId
	JOIN dbo.ItemMultiplier recipeCostUofMToInvUofM ON recipeCostUofMToInvUofM.dm_itemid = i.dm_itemId
		AND recipeCostUofMToInvUofM.src = recipeCost.UofMId
		AND recipeCostUofMToInvUofM.dst =
			CASE 
				WHEN i.dm_Type = 2 AND ISNULL(i.dm_AvailableInInventory, 0) = 0 THEN i.dm_YieldUofM
				ELSE ISNULL(il.dm_InventoryUofM, i.dm_InventoryUofM)
			END
	JOIN dbo.ItemMultiplier recipeCostUofMToYieldUofM ON recipeCostUofMToYieldUofM.dm_itemid = i.dm_itemId
		AND recipeCostUofMToYieldUofM.src = recipeCost.UofMId
		AND recipeCostUofMToYieldUofM.dst = i.dm_YieldUofM
"
let values = parseQuery query1

