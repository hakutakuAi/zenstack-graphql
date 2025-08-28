import type { GraphQLResolveInfo, FieldNode, SelectionNode, GraphQLCompositeType, GraphQLField, GraphQLNamedType } from 'graphql'
import { getNamedType, isCompositeType } from 'graphql'
import { getArgumentValues } from 'graphql/execution/values'

export interface ResolveTree {
	name: string
	alias: string
	args: {
		[str: string]: unknown
	}
	fieldsByTypeName: FieldsByTypeName
}

export interface FieldsByTypeName {
	[str: string]: {
		[str: string]: ResolveTree
	}
}

export interface ParsedField {
	name: string
	selections: string[]
	relations: { [key: string]: ParsedField }
}

function getArgVal(resolveInfo: GraphQLResolveInfo, argument: any) {
	if (argument.kind === 'Variable') {
		return resolveInfo.variableValues[argument.name.value]
	} else if (argument.kind === 'BooleanValue') {
		return argument.value
	}
}

function argNameIsIf(arg: any): boolean {
	return arg && arg.name ? arg.name.value === 'if' : false
}

function skipField(resolveInfo: GraphQLResolveInfo, { directives = [] }: SelectionNode) {
	let skip = false
	directives.forEach((directive) => {
		const directiveName = directive.name.value
		if (Array.isArray(directive.arguments)) {
			const ifArgumentAst = directive.arguments.find(argNameIsIf)
			if (ifArgumentAst) {
				const argumentValueAst = ifArgumentAst.value
				if (directiveName === 'skip') {
					skip = skip || getArgVal(resolveInfo, argumentValueAst)
				} else if (directiveName === 'include') {
					skip = skip || !getArgVal(resolveInfo, argumentValueAst)
				}
			}
		}
	})
	return skip
}

function getFieldFromAST<TContext>(ast: any, parentType: GraphQLCompositeType): GraphQLField<GraphQLCompositeType, TContext> | undefined {
	if (ast.kind === 'Field') {
		const fieldNode: FieldNode = ast
		const fieldName = fieldNode.name.value
		if (parentType && 'getFields' in parentType) {
			return (parentType as any).getFields()[fieldName]
		}
	}
	return undefined
}

function fieldTreeFromAST(
	inASTs: ReadonlyArray<SelectionNode> | SelectionNode,
	resolveInfo: GraphQLResolveInfo,
	initTree: FieldsByTypeName = {},
	parentType: GraphQLCompositeType
): FieldsByTypeName {
	const { variableValues } = resolveInfo
	const fragments = resolveInfo.fragments || {}
	const asts: ReadonlyArray<SelectionNode> = Array.isArray(inASTs) ? inASTs : [inASTs]

	if (!initTree[parentType.name]) {
		initTree[parentType.name] = {}
	}

	return asts.reduce((tree, selectionVal: SelectionNode) => {
		if (skipField(resolveInfo, selectionVal)) {
			return tree
		}

		if (selectionVal.kind === 'Field') {
			const val: FieldNode = selectionVal
			const name = val.name.value
			const isReserved = name[0] === '_' && name[1] === '_' && name !== '__id'

			if (!isReserved) {
				const alias: string = val.alias && val.alias.value ? val.alias.value : name
				const field = getFieldFromAST(val, parentType)

				if (field != null) {
					const fieldGqlTypeOrUndefined = getNamedType(field.type)
					if (fieldGqlTypeOrUndefined) {
						const fieldGqlType: GraphQLNamedType = fieldGqlTypeOrUndefined
						const args = getArgumentValues(field as any, val, variableValues) || {}

						if (parentType.name && tree[parentType.name] && !tree[parentType.name]?.[alias]) {
							const newTreeRoot: ResolveTree = {
								name,
								alias,
								args,
								fieldsByTypeName: isCompositeType(fieldGqlType)
									? {
											[fieldGqlType.name]: {},
									  }
									: {},
							}
							tree[parentType.name]![alias] = newTreeRoot
						}

						const selectionSet = val.selectionSet
						if (selectionSet != null && isCompositeType(fieldGqlType)) {
							const newParentType: GraphQLCompositeType = fieldGqlType
							if (tree[parentType.name] && tree[parentType.name]?.[alias]) {
								fieldTreeFromAST(selectionSet.selections, resolveInfo, tree[parentType.name]![alias]!.fieldsByTypeName, newParentType)
							}
						}
					}
				}
			}
		} else if (selectionVal.kind === 'FragmentSpread') {
			const val = selectionVal
			const name = val.name && val.name.value
			const fragment = fragments[name]

			if (fragment) {
				let fragmentType: GraphQLNamedType | null | undefined = parentType
				if (fragment.typeCondition) {
					const { schema } = resolveInfo
					const typeName = fragment.typeCondition.name.value
					fragmentType = schema.getType(typeName)
				}

				if (fragmentType && isCompositeType(fragmentType)) {
					const newParentType: GraphQLCompositeType = fragmentType
					fieldTreeFromAST(fragment.selectionSet.selections, resolveInfo, tree, newParentType)
				}
			}
		} else if (selectionVal.kind === 'InlineFragment') {
			const val = selectionVal
			const fragment = val
			let fragmentType: GraphQLNamedType | null | undefined = parentType

			if (fragment.typeCondition) {
				const { schema } = resolveInfo
				const typeName = fragment.typeCondition.name.value
				fragmentType = schema.getType(typeName)
			}

			if (fragmentType && isCompositeType(fragmentType)) {
				const newParentType: GraphQLCompositeType = fragmentType
				fieldTreeFromAST(fragment.selectionSet.selections, resolveInfo, tree, newParentType)
			}
		}

		return tree
	}, initTree)
}

function firstKey(obj: object) {
	for (const key in obj) {
		if (Object.prototype.hasOwnProperty.call(obj, key)) {
			return key
		}
	}
}

export function parseResolveInfo(resolveInfo: GraphQLResolveInfo): ResolveTree | null {
	const fieldNodes: ReadonlyArray<FieldNode> = (resolveInfo as any).fieldNodes || (resolveInfo as any).fieldASTs
	const { parentType } = resolveInfo

	if (!fieldNodes) {
		throw new Error('No fieldNodes provided!')
	}

	const tree = fieldTreeFromAST(fieldNodes, resolveInfo, {}, parentType)
	const typeKey = firstKey(tree)

	if (!typeKey) {
		return null
	}

	const fields = tree[typeKey]
	if (!fields) {
		return null
	}
	
	const fieldKey = firstKey(fields)

	if (!fieldKey) {
		return null
	}

	return fields[fieldKey] || null
}

export function getFieldSelections(resolveInfo: GraphQLResolveInfo): string[] {
	const parsed = parseResolveInfo(resolveInfo)
	if (!parsed) return []

	const selections: string[] = []

	function extractSelections(tree: ResolveTree) {
		selections.push(tree.name)
		
		Object.values(tree.fieldsByTypeName).forEach((fields) => {
			Object.values(fields).forEach((field) => {
				extractSelections(field)
			})
		})
	}

	extractSelections(parsed)
	return [...new Set(selections)]
}

export function buildPrismaInclude(resolveInfo: GraphQLResolveInfo, relations: string[] = []): any {
	const parsed = parseResolveInfo(resolveInfo)
	if (!parsed) return {}

	const include: any = {}

	function processField(tree: ResolveTree) {
		Object.values(tree.fieldsByTypeName).forEach((fields) => {
			Object.values(fields).forEach((field) => {
				if (relations.includes(field.name)) {
					include[field.name] = true
					
					if (Object.keys(field.fieldsByTypeName).length > 0) {
						const nestedInclude = {}
						processNestedField(field, nestedInclude)
						if (Object.keys(nestedInclude).length > 0) {
							include[field.name] = { include: nestedInclude }
						}
					}
				} else {
					processField(field)
				}
			})
		})
	}

	function processNestedField(tree: ResolveTree, target: any) {
		Object.values(tree.fieldsByTypeName).forEach((fields) => {
			Object.values(fields).forEach((field) => {
				if (relations.includes(field.name)) {
					target[field.name] = true
					
					if (Object.keys(field.fieldsByTypeName).length > 0) {
						const nestedInclude = {}
						processNestedField(field, nestedInclude)
						if (Object.keys(nestedInclude).length > 0) {
							target[field.name] = { include: nestedInclude }
						}
					}
				} else {
					processNestedField(field, target)
				}
			})
		})
	}

	processField(parsed)
	return include
}