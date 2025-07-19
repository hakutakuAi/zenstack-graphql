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

export const VALID_SCALAR_VALUES = Object.values(SCALAR_TYPES)

export const scalarTypesSchema = z.record(z.string(), z.string()).refine((types) => Object.values(types).every((type) => VALID_SCALAR_VALUES.includes(type)), {
	message: `Invalid scalar type mapping. Valid types: ${VALID_SCALAR_VALUES.join(', ')}`,
})
