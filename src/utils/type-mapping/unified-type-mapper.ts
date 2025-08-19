import { DataModelField, DataModel, Enum } from '@zenstackhq/sdk/ast'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { SchemaProcessor } from '@utils/schema/schema-processor'
import { NormalizedOptions } from '@utils/config'

import { OutputFormat } from '@utils/constants'

export interface TypeMappingConfig {
	[prismaType: string]: {
		graphql: string
		typescript: string
		graphqlScalar?: string
	}
}

export const DEFAULT_TYPE_MAPPING: TypeMappingConfig = {
	String: {
		graphql: 'String',
		typescript: 'string',
	},
	Int: {
		graphql: 'Int',
		typescript: 'number',
	},
	BigInt: {
		graphql: 'Int',
		typescript: 'number',
	},
	Float: {
		graphql: 'Float',
		typescript: 'number',
	},
	Decimal: {
		graphql: 'Float',
		typescript: 'number',
	},
	Boolean: {
		graphql: 'Boolean',
		typescript: 'boolean',
	},
	DateTime: {
		graphql: 'Date',
		typescript: 'Date',
	},
	Json: {
		graphql: 'GraphQLJSON',
		typescript: 'any',
		graphqlScalar: 'JSON',
	},
	Bytes: {
		graphql: 'String',
		typescript: 'string',
	},
}

export enum FieldTypeCategory {
	SCALAR = 'scalar',
	ENUM = 'enum',
	OBJECT = 'object',
}

export class UnifiedTypeMapper {
	private readonly typeMapping: TypeMappingConfig
	private readonly typeFormatter: TypeFormatter
	private readonly modelMap: ReadonlyMap<string, DataModel>
	private readonly customModelNameMap: Map<string, string>
	private readonly enumMap: ReadonlyMap<string, Enum>
	private readonly schemaProcessor: SchemaProcessor
	private readonly options: NormalizedOptions

	constructor(
		typeFormatter: TypeFormatter,
		models: DataModel[] = [],
		enums: Enum[] = [],
		options: NormalizedOptions,
		customMapping: Partial<TypeMappingConfig> = {},
	) {
		this.typeFormatter = typeFormatter
		this.modelMap = new Map(models.map((model) => [model.name, model]))
		this.enumMap = new Map(enums.map((enum_) => [enum_.name, enum_]))
		this.customModelNameMap = new Map()
		this.schemaProcessor = new SchemaProcessor()
		this.options = options

		for (const model of models) {
			const processor = this.schemaProcessor.model(model)
			const customName = processor.name()
			if (customName !== model.name) {
				this.customModelNameMap.set(model.name, customName)
			}
		}

		const filteredMapping = Object.fromEntries(Object.entries(customMapping).filter(([_, value]) => value !== undefined)) as TypeMappingConfig
		this.typeMapping = { ...DEFAULT_TYPE_MAPPING, ...filteredMapping }
	}

	mapFieldType(field: DataModelField, format?: OutputFormat): string | null {
		const targetFormat = format || OutputFormat.GRAPHQL

		try {
			const baseType = this.getBaseType(field, targetFormat)
			return this.formatType(baseType, field.type.array, !field.type.optional, targetFormat)
		} catch {
			return null
		}
	}

	mapScalarType(prismaType: string, format: OutputFormat): string {
		const mapping = this.typeMapping[prismaType]
		if (!mapping) {
			return format === OutputFormat.GRAPHQL ? this.typeFormatter.formatTypeName(prismaType) : this.typeFormatter.formatTypeName(prismaType).toLowerCase()
		}

		return format === OutputFormat.GRAPHQL ? mapping.graphql : mapping.typescript
	}

	getGraphQLScalarName(prismaType: string): string | undefined {
		const mapping = this.typeMapping[prismaType]
		return mapping?.graphqlScalar
	}

	private getBaseType(field: DataModelField, format: OutputFormat): string {
		if (field.type.type) {
			if (field.type.type in this.options.scalarTypes) {
				const customType = this.options.scalarTypes[field.type.type]
				return customType || this.mapScalarType(field.type.type, format)
			}

			return this.mapScalarType(field.type.type, format)
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

		return format === OutputFormat.GRAPHQL ? 'String' : 'string'
	}

	private formatType(baseType: string, isArray: boolean, isRequired: boolean, format: OutputFormat): string {
		let type = baseType

		if (isArray) {
			type = format === OutputFormat.GRAPHQL ? `[${baseType}]` : `${baseType}[]`
		}

		if (format === OutputFormat.GRAPHQL && isRequired) {
			type = isArray ? `[${baseType}!]!` : `${baseType}!`
		}

		return type
	}

	getSupportedTypes(): string[] {
		return Object.keys(this.typeMapping)
	}

	hasTypeMapping(prismaType: string): boolean {
		return prismaType in this.typeMapping
	}

	addCustomMapping(prismaType: string, mapping: TypeMappingConfig[string]): void {
		this.typeMapping[prismaType] = mapping
	}

	getFieldDecoratorArgs(field: DataModelField, format: OutputFormat): string[] {
		if (format !== OutputFormat.GRAPHQL) {
			return []
		}

		const fieldType = this.getBaseType(field, format)
		const args = [`() => ${fieldType}`]

		if (field.type.optional) {
			args.push('{ nullable: true }')
		}

		return args
	}

	getPropertyTypeString(field: DataModelField, format: OutputFormat): string {
		const baseType = this.getBaseType(field, format)
		const suffix = field.type.optional ? '?' : format === OutputFormat.TYPE_GRAPHQL ? '!' : ''
		const finalType = field.type.array ? (format === OutputFormat.GRAPHQL ? `${baseType}[]` : `${baseType}[]`) : baseType

		return `${field.name}${suffix}: ${finalType}`
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
		return Object.values(this.typeMapping).some((mapping) => mapping.graphql === type || mapping.typescript === type)
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
