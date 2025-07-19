import { SCALAR_TYPES } from '@utils/config/constants'
import { DataModel, DataModelField, Enum } from '@zenstackhq/sdk/ast'
import type { DMMF } from '@prisma/generator-helper'

type GraphQLTypeModifiers = {
	isRequired?: boolean
	isList?: boolean
}

export class TypeMapper {
	private readonly models: ReadonlyMap<string, DataModel>
	private readonly enums: ReadonlyMap<string, Enum>

	constructor(models: DataModel[], enums: Enum[]) {
		this.models = new Map(models.map((model) => [model.name, model]))
		this.enums = new Map(enums.map((enum_) => [enum_.name, enum_]))
	}

	static createFromModelsAndEnums(models: DataModel[], enums: Enum[]): TypeMapper {
		return new TypeMapper(models, enums)
	}

	mapFieldType(field: DataModelField): string | null {
		return field.type.array ? this.mapListType(field) : this.mapSingleType(field)
	}

	private mapListType(field: DataModelField): string | null {
		const typeStr = this.getTypeString(field)
		return typeStr ? this.applyModifiers(typeStr, { isList: true, isRequired: !field.type.optional }) : null
	}

	private mapSingleType(field: DataModelField): string | null {
		const typeStr = this.getTypeString(field)
		if (!typeStr) return null

		return this.applyModifiers(typeStr, { isRequired: !field.type.optional })
	}

	private getTypeString(field: DataModelField): string | null {
		if (field.type.type) {
			if (field.type.type in SCALAR_TYPES) {
				return SCALAR_TYPES[field.type.type]
			}
			return null
		}

		if (field.type.reference) {
			const refName = field.type.reference.ref?.name || ''

			if (this.isEnumType(refName)) {
				return refName
			}

			if (this.isModelType(refName)) {
				return refName
			}
		}

		return null
	}

	mapScalarType(typeStr: string): string | null {
		if (typeStr in SCALAR_TYPES) {
			return SCALAR_TYPES[typeStr as keyof typeof SCALAR_TYPES]
		}

		return this.isEnumType(typeStr) ? typeStr : null
	}

	getRelationFieldType(field: DataModelField): string {
		const typeStr = field.type.reference?.ref?.name || ''
		return this.applyModifiers(typeStr, {
			isList: field.type.array,
			isRequired: !field.type.optional,
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

	isRelationField(field: DataModelField): boolean {
		return !!field.type.reference && this.isModelType(field.type.reference.ref?.name || '')
	}

	getFieldKind(field: DataModelField): 'scalar' | 'object' | 'enum' {
		if (this.isRelationField(field)) return 'object'

		if (field.type.reference && this.isEnumType(field.type.reference.ref?.name || '')) {
			return 'enum'
		}

		return 'scalar'
	}

	private applyModifiers(baseType: string, modifiers: GraphQLTypeModifiers): string {
		if (!baseType) return ''

		return modifiers.isList ? `[${baseType}!]${modifiers.isRequired ? '!' : ''}` : modifiers.isRequired ? `${baseType}!` : baseType
	}
}
