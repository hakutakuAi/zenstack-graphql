export {
	BUILTIN_GRAPHQL_SCALARS as BUILTIN_SCALARS,
	BUILTIN_PRISMA_TYPES,
	EXTENDED_BUILTIN_SCALARS,
	isBuiltinGraphQLScalar,
	isBuiltinPrismaType,
	isExtendedBuiltinScalar,
} from './constants/scalar-constants'

export const GRAPHQL_NAMING_REGEX = /^[A-Z_][A-Za-z0-9_]*$/

export const INPUT_TYPE_SUFFIXES = {
	FILTER: 'FilterInput',
	SORT: 'SortInput',
	CONNECTION: 'Connection',
	EDGE: 'Edge',
} as const

export const COMMON_TYPES = {
	NODE: 'Node',
	PAGE_INFO: 'PageInfo',
	SORT_DIRECTION: 'SortDirection',
} as const

export const GENERATOR_TYPES = {
	SCALAR: 'scalar',
	ENUM: 'enum',
	OBJECT: 'object',
	INPUT: 'input',
	CONNECTION: 'connection',
	RELATION: 'relation',
} as const

export enum OutputFormat {
	GRAPHQL = 'graphql',
	TYPE_GRAPHQL = 'type-graphql',
}

export const OUTPUT_FORMATS = {
	GRAPHQL: OutputFormat.GRAPHQL,
	TYPE_GRAPHQL: OutputFormat.TYPE_GRAPHQL,
} as const
