import { BuiltinType } from '@zenstackhq/sdk/ast'
import { z } from 'zod'

export const SCALAR_TYPES: Record<BuiltinType, string> = {
	String: 'String',
	Int: 'Int',
	BigInt: 'Int',
	Float: 'Float',
	Boolean: 'Boolean',
	DateTime: 'DateTime',
	Json: 'JSON',
	Decimal: 'Decimal',
	Bytes: 'String',
}

export const BUILTIN_SCALAR_VALUES = ['String', 'Int', 'Float', 'Boolean', 'ID']

export const DEFAULT_SCALAR_VALUES = Object.values(SCALAR_TYPES)

export const VALID_SCALAR_VALUES = [...BUILTIN_SCALAR_VALUES, ...DEFAULT_SCALAR_VALUES]

function isValidGraphQLScalarName(name: string): boolean {
	return /^[A-Z_][A-Za-z0-9_]*$/.test(name)
}

export const scalarTypesSchema = z.record(z.string(), z.string()).refine(
	(types) => {
		return Object.values(types).every((type) => {
			const typeLC = type.toLowerCase()
			const isStandardScalar = [...BUILTIN_SCALAR_VALUES, ...DEFAULT_SCALAR_VALUES].some((valid) => valid.toLowerCase() === typeLC)

			if (isStandardScalar) return true

			return isValidGraphQLScalarName(type)
		})
	},
	{
		message: `Invalid scalar type mapping. Scalar types must follow GraphQL naming conventions (start with uppercase letter or underscore, contain only letters, numbers, and underscores). Standard scalars are: ${[...BUILTIN_SCALAR_VALUES, ...DEFAULT_SCALAR_VALUES].join(', ')}`,
	}
)
