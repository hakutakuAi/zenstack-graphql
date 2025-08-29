import type { GraphQLResolveInfo } from 'graphql'

export function buildSelect<TSelect = any>(baseSelect: TSelect, info?: GraphQLResolveInfo): TSelect {
	if (!info) return baseSelect

	return extractSelectFromInfo(baseSelect, info)
}

function extractSelectFromInfo<TSelect>(baseSelect: TSelect, info: GraphQLResolveInfo): TSelect {
	const selections = info.fieldNodes[0]?.selectionSet?.selections
	if (!selections) return baseSelect

	const select = { ...baseSelect }
	const nonRelationFields = new Set(['edges', 'pageInfo', 'totalCount', 'cursor', 'node'])

	for (const selection of selections) {
		if (selection.kind === 'Field' && selection.selectionSet) {
			const fieldName = selection.name.value
			if (!nonRelationFields.has(fieldName) && (baseSelect as any)[fieldName]) {
			}
		}
	}

	return select
}
