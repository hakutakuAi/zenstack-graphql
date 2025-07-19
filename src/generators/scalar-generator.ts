import { ScalarTypeComposer, SchemaComposer } from 'graphql-compose'
import { GraphQLScalarType, GraphQLError } from 'graphql'
import { Kind } from 'graphql/language'
import { BaseGenerator } from '@generators/base-generator'
import { GeneratorContext } from '@types'
import { TypeKind } from '@utils/registry/unified-registry'

export interface ScalarConfig {
	name: string
	description?: string
	serialize: (value: unknown) => unknown
	parseValue: (value: unknown) => unknown
	parseLiteral: (ast: unknown) => unknown
}

export class ScalarGenerator extends BaseGenerator {
	constructor(context: GeneratorContext) {
		super(context)
		if (!context.typeMapper) {
			throw new Error('TypeMapper is required for ScalarGenerator')
		}
	}

	protected override skipGeneration(): boolean {
		return !this.options.generateScalars
	}

	generate(): void {
		if (this.skipGeneration()) {
			return
		}

		this.registerBuiltInScalars()
		this.registerCustomScalars()
	}

	getGeneratedScalars(): string[] {
		return this.registry.getScalarTypes()
	}

	hasScalar(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.SCALAR)
	}

	getScalarComposer(name: string): ScalarTypeComposer | undefined {
		return this.registry.getScalarComposer(name)
	}

	private registerBuiltInScalars(): void {
		const builtInScalars = [this.createDateTimeScalar(), this.createJSONScalar(), this.createDecimalScalar()]
		builtInScalars.forEach((scalar) => this.registerScalar(scalar))
	}

	private registerCustomScalars(): void {
		Object.entries(this.options.scalarTypes).forEach(([prismaType, graphqlType]) => {
			if (!this.isBuiltInScalar(graphqlType) && !this.hasScalar(graphqlType)) {
				const customScalar = this.createCustomScalar(graphqlType, prismaType)
				this.registerScalar(customScalar)
			}
		})
	}

	private createDateTimeScalar(): ScalarConfig {
		return {
			name: 'DateTime',
			description: 'A date-time string at UTC, such as 2007-12-03T10:15:30Z',
			serialize: (value: unknown) => {
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
			parseValue: (value: unknown) => {
				if (typeof value !== 'string') {
					throw new GraphQLError(`Value is not a string: ${value}`)
				}
				const date = new Date(value)
				if (isNaN(date.getTime())) {
					throw new GraphQLError(`Value is not a valid DateTime: ${value}`)
				}
				return date
			},
			parseLiteral: (ast: unknown) => {
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

	private createJSONScalar(): ScalarConfig {
		const parseLiteral = (ast: unknown): unknown => {
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
			serialize: (value: unknown) => {
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
			parseValue: (value: unknown) => {
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

	private createDecimalScalar(): ScalarConfig {
		return {
			name: 'Decimal',
			description: 'An arbitrary-precision Decimal type',
			serialize: (value: unknown) => {
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
			parseValue: (value: unknown) => {
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
			parseLiteral: (ast: unknown) => {
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

	private createCustomScalar(name: string, prismaType: string): ScalarConfig {
		return {
			name,
			description: `Custom scalar type for ${prismaType}`,
			serialize: (value: unknown) => {
				if (value === null || value === undefined) {
					return null
				}
				return String(value)
			},
			parseValue: (value: unknown) => {
				if (typeof value === 'string') {
					return value
				}
				return String(value)
			},
			parseLiteral: (ast: unknown) => {
				const node = ast as { kind: string; value: string }
				if (node.kind === Kind.STRING) {
					return node.value
				}
				throw new GraphQLError(`Can only parse strings to ${name} but got a: ${node.kind}`)
			},
		}
	}

	private registerScalar(config: ScalarConfig): void {
		const scalarType = new GraphQLScalarType({
			name: config.name,
			description: config.description,
			serialize: config.serialize,
			parseValue: config.parseValue,
			parseLiteral: config.parseLiteral as any,
		})

		const scalarComposer = ScalarTypeComposer.createTemp(scalarType)
		this.schemaComposer.set(config.name, scalarComposer)

		this.registry.registerType(config.name, TypeKind.SCALAR, scalarComposer, true)
	}

	private isBuiltInScalar(typeName: string): boolean {
		return this.registry.isBuiltInScalar(typeName)
	}
}
