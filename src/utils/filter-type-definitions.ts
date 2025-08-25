import { FilterFieldDefinition } from '@generators/strategies'

export interface CommonFilterTypeDefinition {
	name: string
	type: 'numeric' | 'string' | 'boolean' | 'datetime'
	baseType: string
	includeComparisons: boolean
	includeStringOperations: boolean
}

export const COMMON_FILTER_TYPES: CommonFilterTypeDefinition[] = [
	{
		name: 'NumericFilterInput',
		type: 'numeric',
		baseType: 'Float',
		includeComparisons: true,
		includeStringOperations: false,
	},
	{
		name: 'StringFilterInput',
		type: 'string',
		baseType: 'String',
		includeComparisons: false,
		includeStringOperations: true,
	},
	{
		name: 'BooleanFilterInput',
		type: 'boolean',
		baseType: 'Boolean',
		includeComparisons: false,
		includeStringOperations: false,
	},
	{
		name: 'DateTimeFilterInput',
		type: 'datetime',
		baseType: 'Date',
		includeComparisons: true,
		includeStringOperations: false,
	},
]

export function createFilterFields(definition: CommonFilterTypeDefinition): FilterFieldDefinition[] {
	const fields: FilterFieldDefinition[] = []

	fields.push(
		{ name: 'equals', type: definition.baseType, nullable: true, description: 'Equal to the given value' },
		{ name: 'not', type: definition.baseType, nullable: true, description: 'Not equal to the given value' },
	)

	if (definition.includeComparisons) {
		fields.push(
			{ name: 'gt', type: definition.baseType, nullable: true, description: 'Greater than the given value' },
			{ name: 'gte', type: definition.baseType, nullable: true, description: 'Greater than or equal to the given value' },
			{ name: 'lt', type: definition.baseType, nullable: true, description: 'Less than the given value' },
			{ name: 'lte', type: definition.baseType, nullable: true, description: 'Less than or equal to the given value' },
		)
	}

	if (definition.includeStringOperations) {
		fields.push(
			{ name: 'in', type: `[${definition.baseType}!]`, nullable: true, description: 'In the given list of values' },
			{ name: 'notIn', type: `[${definition.baseType}!]`, nullable: true, description: 'Not in the given list of values' },
			{ name: 'contains', type: definition.baseType, nullable: true, description: 'Contains the given value' },
			{ name: 'startsWith', type: definition.baseType, nullable: true, description: 'Starts with the given value' },
			{ name: 'endsWith', type: definition.baseType, nullable: true, description: 'Ends with the given value' },
		)
	}

	return fields
}

export function createGraphQLFilterFields(definition: CommonFilterTypeDefinition): Record<string, { type: string; description: string }> {
	const fields: Record<string, { type: string; description: string }> = {}

	fields.equals = { type: definition.baseType, description: 'Equal to the given value' }
	fields.not = { type: definition.baseType, description: 'Not equal to the given value' }

	if (definition.includeComparisons) {
		fields.gt = { type: definition.baseType, description: 'Greater than the given value' }
		fields.gte = { type: definition.baseType, description: 'Greater than or equal to the given value' }
		fields.lt = { type: definition.baseType, description: 'Less than the given value' }
		fields.lte = { type: definition.baseType, description: 'Less than or equal to the given value' }
	}

	if (definition.includeStringOperations) {
		fields.in = { type: `[${definition.baseType}!]`, description: 'In the given list of values' }
		fields.notIn = { type: `[${definition.baseType}!]`, description: 'Not in the given list of values' }
		fields.contains = { type: definition.baseType, description: 'Contains the given value' }
		fields.startsWith = { type: definition.baseType, description: 'Starts with the given value' }
		fields.endsWith = { type: definition.baseType, description: 'Ends with the given value' }
	}

	return fields
}
