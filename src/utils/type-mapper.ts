import type { DMMF } from '@zenstackhq/sdk/prisma'
import { match } from 'ts-pattern'
import { ScalarTypeKey, SCALAR_TYPES } from './constants'

type GraphQLTypeModifiers = {
	isRequired?: boolean
	isList?: boolean
}

export class TypeMapper {
	private readonly models: ReadonlyMap<string, DMMF.Model>
	private readonly enums: ReadonlyMap<string, DMMF.DatamodelEnum>

	constructor(dmmfModels: readonly DMMF.Model[], dmmfEnums: readonly DMMF.DatamodelEnum[]) {
		this.models = new Map(dmmfModels.map((model) => [model.name, model]))
		this.enums = new Map(dmmfEnums.map((enum_) => [enum_.name, enum_]))
	}

	mapFieldType(field: DMMF.Field): string | null {
		if (field.isList) {
			const baseType = this.mapScalarType(field.type)
			return baseType ? this.applyModifiers(baseType, { isList: true, isRequired: true }) : null
		}

		return match<string, string | null>(field.type)
			.with('String', () => this.applyModifiers(SCALAR_TYPES.String, { isRequired: field.isRequired }))
			.with('Int', () => this.applyModifiers(SCALAR_TYPES.Int, { isRequired: field.isRequired }))
			.with('BigInt', () => this.applyModifiers(SCALAR_TYPES.BigInt, { isRequired: field.isRequired }))
			.with('Float', () => this.applyModifiers(SCALAR_TYPES.Float, { isRequired: field.isRequired }))
			.with('Boolean', () => this.applyModifiers(SCALAR_TYPES.Boolean, { isRequired: field.isRequired }))
			.with('DateTime', () => this.applyModifiers(SCALAR_TYPES.DateTime, { isRequired: field.isRequired }))
			.with('Json', () => this.applyModifiers(SCALAR_TYPES.Json, { isRequired: field.isRequired }))
			.with('Decimal', () => this.applyModifiers(SCALAR_TYPES.Decimal, { isRequired: field.isRequired }))
			.with('Bytes', () => this.applyModifiers(SCALAR_TYPES.Bytes, { isRequired: field.isRequired }))
			.when(
				(type) => this.isEnumType(type),
				(type) => this.applyModifiers(type, { isRequired: field.isRequired })
			)
			.when(
				(type) => this.isModelType(type),
				(type) => this.applyModifiers(type, { isRequired: field.isRequired })
			)
			.otherwise(() => null)
	}

	mapScalarType(type: string): string | null {
		if (type in SCALAR_TYPES) {
			return SCALAR_TYPES[type as ScalarTypeKey]
		}

		if (this.isEnumType(type)) {
			return type
		}

		return null
	}

	getRelationFieldType(field: DMMF.Field): string {
		return this.applyModifiers(field.type, {
			isList: field.isList,
			isRequired: field.isRequired,
		})
	}

	isScalarType(type: string): boolean {
		return type in SCALAR_TYPES
	}

	isEnumType(type: string): boolean {
		return this.enums.has(type)
	}

	isModelType(type: string): boolean {
		return this.models.has(type)
	}

	isRelationField(field: DMMF.Field): boolean {
		return field.kind === 'object' && !!field.relationName
	}

	getFieldKind(field: DMMF.Field): 'scalar' | 'object' | 'enum' {
		if (field.kind === 'object') return 'object'
		if (this.isEnumType(field.type)) return 'enum'
		return 'scalar'
	}

	private applyModifiers(baseType: string, modifiers: GraphQLTypeModifiers): string {
		if (!baseType) return ''

		if (modifiers.isList) {
			return `[${baseType}!]${modifiers.isRequired ? '!' : ''}`
		}

		return modifiers.isRequired ? `${baseType}!` : baseType
	}

	static createFromDMMF(dmmf: DMMF.Document): TypeMapper {
		return new TypeMapper(dmmf.datamodel.models, dmmf.datamodel.enums)
	}
}
