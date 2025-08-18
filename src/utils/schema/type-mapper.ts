import { SCALAR_TYPES } from '@utils/config'
import { DataModel, DataModelField, Enum } from '@zenstackhq/sdk/ast'
import { SchemaProcessor } from './schema-processor'
import { NormalizedOptions } from '@utils/config'

export enum FieldTypeCategory {
	SCALAR = 'scalar',
	ENUM = 'enum',
	OBJECT = 'object',
}

export class TypeMapper {
	private readonly modelMap: ReadonlyMap<string, DataModel>
	private readonly customModelNameMap: Map<string, string>
	private readonly enumMap: ReadonlyMap<string, Enum>
	private readonly schemaProcessor: SchemaProcessor
	private readonly options: NormalizedOptions
	private readonly customScalarTypes: Record<string, string>

	constructor(models: DataModel[], enums: Enum[], options: NormalizedOptions) {
		this.modelMap = new Map(models.map((model) => [model.name, model]))
		this.enumMap = new Map(enums.map((enum_) => [enum_.name, enum_]))
		this.customModelNameMap = new Map()
		this.schemaProcessor = new SchemaProcessor()
		this.options = options
		this.customScalarTypes = options.scalarTypes

		for (const model of models) {
			const processor = this.schemaProcessor.model(model)
			const customName = processor.name()
			if (customName !== model.name) {
				this.customModelNameMap.set(model.name, customName)
			}
		}
	}

	static createFromModelsAndEnums(models: DataModel[], enums: Enum[], options: NormalizedOptions): TypeMapper {
		return new TypeMapper(models, enums, options)
	}

	mapFieldType(field: DataModelField): string | null {
		const baseType = this.resolveBaseType(field)
		if (!baseType) return null

		return this.formatGraphQLType(baseType, field.type.array, !field.type.optional)
	}

	private resolveBaseType(field: DataModelField): string | null {
		if (field.type.type) {
			if (field.type.type in this.customScalarTypes) {
				const customType = this.customScalarTypes[field.type.type]
				return customType || null
			}

			if (field.type.type in SCALAR_TYPES) {
				return SCALAR_TYPES[field.type.type]
			}
		}

		if (field.type.reference) {
			const refName = field.type.reference.ref?.name || ''

			if (this.isEnumType(refName)) {
				return refName
			}

			if (this.isModelType(refName)) {
				if (this.customModelNameMap.has(refName)) {
					return this.customModelNameMap.get(refName)!
				}
				return refName
			}
		}

		return null
	}

	mapScalarType(typeStr: string): string | null {
		if (typeStr in this.customScalarTypes) {
			const customType = this.customScalarTypes[typeStr]
			return customType || null
		}

		if (typeStr in SCALAR_TYPES) {
			return SCALAR_TYPES[typeStr as keyof typeof SCALAR_TYPES]
		}

		return this.isEnumType(typeStr) ? typeStr : null
	}

	getRelationFieldType(field: DataModelField): string {
		let typeStr = field.type.reference?.ref?.name || ''

		if (this.customModelNameMap.has(typeStr)) {
			typeStr = this.customModelNameMap.get(typeStr)!
		}

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
		if (this.modelMap.has(type)) {
			return true
		}

		for (const [, customName] of this.customModelNameMap.entries()) {
			if (customName === type) {
				return true
			}
		}

		return false
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
