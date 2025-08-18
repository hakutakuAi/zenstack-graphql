// GraphQL naming patterns
export const GRAPHQL_NAMING_REGEX = /^[A-Z_][A-Za-z0-9_]*$/

// Common field suffixes
export const INPUT_TYPE_SUFFIXES = {
	FILTER: 'FilterInput',
	SORT: 'SortInput',
	CONNECTION: 'Connection',
	EDGE: 'Edge'
} as const

// Built-in GraphQL scalars
export const BUILTIN_SCALARS = ['String', 'Int', 'Float', 'Boolean', 'ID'] as const

// Common GraphQL type names
export const COMMON_TYPES = {
	NODE: 'Node',
	PAGE_INFO: 'PageInfo',
	SORT_DIRECTION: 'SortDirection'
} as const