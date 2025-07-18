import type { DMMF } from '@zenstackhq/sdk/prisma'
import { ScalarTypeKey, SCALAR_TYPES } from '@utils/config/constants'

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
		return field.isList ? this.mapListType(field) : this.mapSingleType(field)
	}

	private mapListType(field: DMMF.Field): string | null {
		const baseType = this.mapScalarType(field.type)
		return baseType ? this.applyModifiers(baseType, { isList: true, isRequired: true }) : null
	}

	private mapSingleType(field: DMMF.Field): string | null {
		if (field.type in SCALAR_TYPES) {
			return this.applyModifiers(SCALAR_TYPES[field.type as ScalarTypeKey], { isRequired: field.isRequired })
		}

		if (this.isEnumType(field.type)) {
			return this.applyModifiers(field.type, { isRequired: field.isRequired })
		}

		if (this.isModelType(field.type)) {
			return this.applyModifiers(field.type, { isRequired: field.isRequired })
		}

		return null
	}

	mapScalarType(type: string): string | null {
		if (type in SCALAR_TYPES) {
			return SCALAR_TYPES[type as ScalarTypeKey]
		}

		return this.isEnumType(type) ? type : null
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

		return modifiers.isList ? `[${baseType}!]${modifiers.isRequired ? '!' : ''}` : modifiers.isRequired ? `${baseType}!` : baseType
	}

	static createFromDMMF(dmmf: DMMF.Document): TypeMapper {
		return new TypeMapper(dmmf.datamodel.models, dmmf.datamodel.enums)
	}
}
