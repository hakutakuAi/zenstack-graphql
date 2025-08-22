import { GraphQLScalarType, GraphQLError } from 'graphql'
import { Kind } from 'graphql/language'

export const BUILTIN_GRAPHQL_SCALARS = ['String', 'Int', 'Float', 'Boolean', 'ID'] as const
export const BUILTIN_PRISMA_TYPES = ['DateTime', 'Json', 'Decimal', 'Bytes'] as const
export const EXTENDED_BUILTIN_SCALARS = [...BUILTIN_GRAPHQL_SCALARS, 'DateTime', 'JSON', 'Decimal'] as const

export type BuiltinGraphQLScalar = (typeof BUILTIN_GRAPHQL_SCALARS)[number]
export type BuiltinPrismaType = (typeof BUILTIN_PRISMA_TYPES)[number]
export type ExtendedBuiltinScalar = (typeof EXTENDED_BUILTIN_SCALARS)[number]

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

export interface ScalarConfig {
	name: string
	description: string
	serialize: (value: any) => any
	parseValue: (value: any) => any
	parseLiteral: (ast: any) => any
}

export interface UnifiedScalarDefinition {
	prismaType: BuiltinPrismaType
	graphqlType: string
	typescriptType: string
	description: string
	createGraphQLConfig: () => ScalarConfig
	createTypeScriptType: () => string
}

export function createDateTimeScalarConfig(): ScalarConfig {
	return {
		name: 'DateTime',
		description: 'A date-time string at UTC, such as 2007-12-03T10:15:30Z',
		serialize: (value: any): string => {
			if (value instanceof Date) {
				if (isNaN(value.getTime())) {
					throw new GraphQLError(`Value is not a valid DateTime: ${value}`)
				}
				return value.toISOString()
			}
			if (typeof value === 'string') {
				const date = new Date(value)
				if (isNaN(date.getTime())) {
					throw new GraphQLError(`Value is not a valid DateTime: ${value}`)
				}
				return date.toISOString()
			}
			throw new GraphQLError(`Value is not a valid DateTime: ${value}`)
		},
		parseValue: (value: any): Date => {
			if (typeof value !== 'string') {
				throw new GraphQLError(`Value is not a string: ${value}`)
			}
			const date = new Date(value)
			if (isNaN(date.getTime())) {
				throw new GraphQLError(`Value is not a valid DateTime: ${value}`)
			}
			return date
		},
		parseLiteral: (ast: any): Date => {
			const node = ast as { kind: string; value: string }
			if (node.kind !== Kind.STRING) {
				throw new GraphQLError(`Can only parse strings to DateTime but got a: ${node.kind}`)
			}
			const date = new Date(node.value)
			if (isNaN(date.getTime())) {
				throw new GraphQLError(`Value is not a valid DateTime: ${node.value}`)
			}
			return date
		},
	}
}

export function createJSONScalarConfig(): ScalarConfig {
	const parseLiteral = (ast: any): any => {
		const node = ast as { kind: string; value: string; fields?: any[] }

		switch (node.kind) {
			case Kind.STRING:
				try {
					return JSON.parse(node.value)
				} catch {
					return node.value
				}
			case Kind.INT:
				return parseInt(node.value, 10)
			case Kind.FLOAT:
				return parseFloat(node.value)
			case Kind.BOOLEAN:
				return node.value
			case Kind.NULL:
				return null
			case Kind.OBJECT:
				return (
					node.fields?.reduce((obj: Record<string, unknown>, field: any) => {
						obj[field.name.value] = parseLiteral(field.value)
						return obj
					}, {}) || {}
				)
			case Kind.LIST:
				return (node as any).values?.map((value: any) => parseLiteral(value)) || []
			default:
				throw new GraphQLError(`Cannot parse literal of kind: ${node.kind}`)
		}
	}

	return {
		name: 'JSON',
		description: 'The `JSON` scalar type represents JSON values as specified by ECMA-404',
		serialize: (value: any): any => {
			if (value === null || value === undefined) {
				return null
			}
			if (typeof value === 'object' || Array.isArray(value)) {
				return value
			}
			if (typeof value === 'string') {
				try {
					return JSON.parse(value)
				} catch {
					return value
				}
			}
			return value
		},
		parseValue: (value: any): any => {
			if (typeof value === 'string') {
				try {
					return JSON.parse(value)
				} catch {
					throw new GraphQLError(`Value is not a valid JSON string: ${value}`)
				}
			}
			return value
		},
		parseLiteral,
	}
}

export function createDecimalScalarConfig(): ScalarConfig {
	return {
		name: 'Decimal',
		description: 'An arbitrary-precision Decimal type',
		serialize: (value: any): string | null => {
			if (value === null || value === undefined) {
				return null
			}
			if (typeof value === 'number') {
				return value.toString()
			}
			if (typeof value === 'string') {
				const num = parseFloat(value)
				if (isNaN(num)) {
					throw new GraphQLError(`Value is not a valid Decimal: ${value}`)
				}
				return value
			}
			if (typeof value === 'object' && value !== null && 'toString' in value && typeof value.toString === 'function') {
				return value.toString()
			}
			throw new GraphQLError(`Value is not a valid Decimal: ${value}`)
		},
		parseValue: (value: any): string => {
			if (typeof value === 'number') {
				return value.toString()
			}
			if (typeof value === 'string') {
				const num = parseFloat(value)
				if (isNaN(num)) {
					throw new GraphQLError(`Value is not a valid Decimal: ${value}`)
				}
				return value
			}
			throw new GraphQLError(`Value is not a valid Decimal: ${value}`)
		},
		parseLiteral: (ast: any): string => {
			const node = ast as { kind: string; value: string }
			if (node.kind === Kind.STRING || node.kind === Kind.INT || node.kind === Kind.FLOAT) {
				const num = parseFloat(node.value)
				if (isNaN(num)) {
					throw new GraphQLError(`Value is not a valid Decimal: ${node.value}`)
				}
				return node.value
			}
			throw new GraphQLError(`Can only parse strings, integers, and floats to Decimal but got a: ${node.kind}`)
		},
	}
}

export function createCustomScalarConfig(name: string, prismaType: string): ScalarConfig {
	return {
		name,
		description: `Custom scalar type for ${prismaType}`,
		serialize: (value: any): string | null => {
			if (value === null || value === undefined) {
				return null
			}
			return String(value)
		},
		parseValue: (value: any): string => {
			if (typeof value === 'string') {
				return value
			}
			return String(value)
		},
		parseLiteral: (ast: any): string => {
			const node = ast as { kind: string; value: string }
			if (node.kind === Kind.STRING) {
				return node.value
			}
			throw new GraphQLError(`Can only parse strings to ${name} but got a: ${node.kind}`)
		},
	}
}

export const UNIFIED_SCALAR_DEFINITIONS: UnifiedScalarDefinition[] = [
	{
		prismaType: 'DateTime',
		graphqlType: 'DateTime',
		typescriptType: 'Date',
		description: 'A date-time string at UTC, such as 2007-12-03T10:15:30Z',
		createGraphQLConfig: createDateTimeScalarConfig,
		createTypeScriptType: () => 'Date',
	},
	{
		prismaType: 'Json',
		graphqlType: 'JSON',
		typescriptType: 'any',
		description: 'The `JSON` scalar type represents JSON values as specified by ECMA-404',
		createGraphQLConfig: createJSONScalarConfig,
		createTypeScriptType: () => 'any',
	},
	{
		prismaType: 'Decimal',
		graphqlType: 'Decimal',
		typescriptType: 'number',
		description: 'An arbitrary-precision Decimal type',
		createGraphQLConfig: createDecimalScalarConfig,
		createTypeScriptType: () => 'number',
	},
] as const

export function createGraphQLScalarType(config: ScalarConfig): GraphQLScalarType {
	return new GraphQLScalarType({
		name: config.name,
		description: config.description,
		serialize: config.serialize,
		parseValue: config.parseValue,
		parseLiteral: config.parseLiteral as any,
	})
}

export function isBuiltinGraphQLScalar(typeName: string): typeName is BuiltinGraphQLScalar {
	return BUILTIN_GRAPHQL_SCALARS.includes(typeName as BuiltinGraphQLScalar)
}

export function isBuiltinPrismaType(typeName: string): typeName is BuiltinPrismaType {
	return BUILTIN_PRISMA_TYPES.includes(typeName as BuiltinPrismaType)
}

export function isExtendedBuiltinScalar(typeName: string): typeName is ExtendedBuiltinScalar {
	return EXTENDED_BUILTIN_SCALARS.includes(typeName as ExtendedBuiltinScalar)
}

export function getScalarDefinition(prismaType: string): UnifiedScalarDefinition | undefined {
	return UNIFIED_SCALAR_DEFINITIONS.find((def) => def.prismaType === prismaType)
}
