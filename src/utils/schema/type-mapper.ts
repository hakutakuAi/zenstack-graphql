import { SCALAR_TYPES } from '@utils/config/constants'
import { DataModel, DataModelField, Enum } from '@zenstackhq/sdk/ast'

export enum FieldTypeCategory {
	SCALAR = 'scalar',
	ENUM = 'enum',
	OBJECT = 'object',
}

export class TypeMapper {
	private readonly modelMap: ReadonlyMap<string, DataModel>
	private readonly enumMap: ReadonlyMap<string, Enum>

	constructor(models: DataModel[], enums: Enum[]) {
		this.modelMap = new Map(models.map((model) => [model.name, model]))
		this.enumMap = new Map(enums.map((enum_) => [enum_.name, enum_]))
	}

	static createFromModelsAndEnums(models: DataModel[], enums: Enum[]): TypeMapper {
		return new TypeMapper(models, enums)
	}

	mapFieldType(field: DataModelField): string | null {
		const baseType = this.resolveBaseType(field)
		if (!baseType) return null

		return this.formatGraphQLType(baseType, field.type.array, !field.type.optional)
	}

	private resolveBaseType(field: DataModelField): string | null {
		if (field.type.type && field.type.type in SCALAR_TYPES) {
			return SCALAR_TYPES[field.type.type]
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
		return this.formatGraphQLType(typeStr, field.type.array, !field.type.optional)
	}

	private formatGraphQLType(baseType: string, isList: boolean, isRequired: boolean): string {
		if (!baseType) return ''

		if (isList) {
			return `[${baseType}!]${isRequired ? '!' : ''}`
		}

		return isRequired ? `${baseType}!` : baseType
	}

	isScalarType(type: string): boolean {
		return Object.values(SCALAR_TYPES).includes(type)
	}

	isEnumType(type: string): boolean {
		return this.enumMap.has(type)
	}

	isModelType(type: string): boolean {
		return this.modelMap.has(type)
	}

	isRelationField(field: DataModelField): boolean {
		return !!field.type.reference && this.isModelType(field.type.reference.ref?.name || '')
	}

	getFieldTypeCategory(field: DataModelField): FieldTypeCategory {
		if (this.isRelationField(field)) {
			return FieldTypeCategory.OBJECT
		}

		if (field.type.reference && this.isEnumType(field.type.reference.ref?.name || '')) {
			return FieldTypeCategory.ENUM
		}

		return FieldTypeCategory.SCALAR
	}
}
