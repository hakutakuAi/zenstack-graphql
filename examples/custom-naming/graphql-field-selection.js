const { getNamedType, isCompositeType } = require('graphql');
const { getArgumentValues } = require('graphql/execution/values');

function getArgVal(resolveInfo, argument) {
	if (argument.kind === 'Variable') {
		return resolveInfo.variableValues[argument.name.value];
	} else if (argument.kind === 'BooleanValue') {
		return argument.value;
	}
}

function argNameIsIf(arg) {
	return arg && arg.name ? arg.name.value === 'if' : false;
}

function skipField(resolveInfo, { directives = [] }) {
	let skip = false;
	directives.forEach((directive) => {
		const directiveName = directive.name.value;
		if (Array.isArray(directive.arguments)) {
			const ifArgumentAst = directive.arguments.find(argNameIsIf);
			if (ifArgumentAst) {
				const argumentValueAst = ifArgumentAst.value;
				if (directiveName === 'skip') {
					skip = skip || getArgVal(resolveInfo, argumentValueAst);
				} else if (directiveName === 'include') {
					skip = skip || !getArgVal(resolveInfo, argumentValueAst);
				}
			}
		}
	});
	return skip;
}

function getFieldFromAST(ast, parentType) {
	if (ast.kind === 'Field') {
		const fieldNode = ast;
		const fieldName = fieldNode.name.value;
		if (parentType && 'getFields' in parentType) {
			return parentType.getFields()[fieldName];
		}
	}
	return undefined;
}

function fieldTreeFromAST(inASTs, resolveInfo, initTree = {}, parentType) {
	const { variableValues } = resolveInfo;
	const fragments = resolveInfo.fragments || {};
	const asts = Array.isArray(inASTs) ? inASTs : [inASTs];

	if (!initTree[parentType.name]) {
		initTree[parentType.name] = {};
	}

	return asts.reduce((tree, selectionVal) => {
		if (skipField(resolveInfo, selectionVal)) {
			return tree;
		}

		if (selectionVal.kind === 'Field') {
			const val = selectionVal;
			const name = val.name.value;
			const isReserved = name[0] === '_' && name[1] === '_' && name !== '__id';

			if (!isReserved) {
				const alias = val.alias && val.alias.value ? val.alias.value : name;
				const field = getFieldFromAST(val, parentType);

				if (field != null) {
					const fieldGqlTypeOrUndefined = getNamedType(field.type);
					if (fieldGqlTypeOrUndefined) {
						const fieldGqlType = fieldGqlTypeOrUndefined;
						const args = getArgumentValues(field, val, variableValues) || {};

						if (parentType.name && tree[parentType.name] && !tree[parentType.name][alias]) {
							const newTreeRoot = {
								name,
								alias,
								args,
								fieldsByTypeName: isCompositeType(fieldGqlType) ? { [fieldGqlType.name]: {} } : {},
							};
							tree[parentType.name][alias] = newTreeRoot;
						}

						const selectionSet = val.selectionSet;
						if (selectionSet != null && isCompositeType(fieldGqlType)) {
							const newParentType = fieldGqlType;
							if (tree[parentType.name] && tree[parentType.name][alias]) {
								fieldTreeFromAST(selectionSet.selections, resolveInfo, tree[parentType.name][alias].fieldsByTypeName, newParentType);
							}
						}
					}
				}
			}
		}

		return tree;
	}, initTree);
}

function firstKey(obj) {
	for (const key in obj) {
		if (Object.prototype.hasOwnProperty.call(obj, key)) {
			return key;
		}
	}
}

function parseResolveInfo(resolveInfo) {
	const fieldNodes = resolveInfo.fieldNodes || resolveInfo.fieldASTs;
	const { parentType } = resolveInfo;

	if (!fieldNodes) {
		throw new Error('No fieldNodes provided!');
	}

	const tree = fieldTreeFromAST(fieldNodes, resolveInfo, {}, parentType);
	const typeKey = firstKey(tree);

	if (!typeKey) {
		return null;
	}

	const fields = tree[typeKey];
	if (!fields) {
		return null;
	}
	
	const fieldKey = firstKey(fields);

	if (!fieldKey) {
		return null;
	}

	return fields[fieldKey] || null;
}

function buildPrismaInclude(resolveInfo, relations = []) {
	const parsed = parseResolveInfo(resolveInfo);
	if (!parsed) return {};

	const include = {};

	function processField(tree) {
		Object.values(tree.fieldsByTypeName).forEach((fields) => {
			Object.values(fields).forEach((field) => {
				if (relations.includes(field.name)) {
					include[field.name] = true;
					
					if (Object.keys(field.fieldsByTypeName).length > 0) {
						const nestedInclude = {};
						processNestedField(field, nestedInclude);
						if (Object.keys(nestedInclude).length > 0) {
							include[field.name] = { include: nestedInclude };
						}
					}
				} else {
					processField(field);
				}
			});
		});
	}

	function processNestedField(tree, target) {
		Object.values(tree.fieldsByTypeName).forEach((fields) => {
			Object.values(fields).forEach((field) => {
				if (relations.includes(field.name)) {
					target[field.name] = true;
					
					if (Object.keys(field.fieldsByTypeName).length > 0) {
						const nestedInclude = {};
						processNestedField(field, nestedInclude);
						if (Object.keys(nestedInclude).length > 0) {
							target[field.name] = { include: nestedInclude };
						}
					}
				} else {
					processNestedField(field, target);
				}
			});
		});
	}

	processField(parsed);
	return include;
}

module.exports = {
	parseResolveInfo,
	buildPrismaInclude,
	getFieldSelections: function(resolveInfo) {
		const parsed = parseResolveInfo(resolveInfo);
		if (!parsed) return [];

		const selections = [];

		function extractSelections(tree) {
			selections.push(tree.name);
			
			Object.values(tree.fieldsByTypeName).forEach((fields) => {
				Object.values(fields).forEach((field) => {
					extractSelections(field);
				});
			});
		}

		extractSelections(parsed);
		return [...new Set(selections)];
	},
};