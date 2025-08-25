import { AttributeArg, DataModel, DataModelField, DataModelAttribute, DataModelFieldAttribute, Enum } from '@zenstackhq/sdk/ast'
import { TypeFormatter } from './type-formatter'

type AttributeType = DataModelAttribute | DataModelFieldAttribute | undefined
type AttributeGetter<T> = (attr: AttributeType, argName?: string) => T | undefined

export interface ModelAttributeChain {
	getString(argName: string): string | undefined
	getBoolean(argName: string): boolean | undefined
	getNumber(argName: string): number | undefined
	exists(): boolean
	isIgnored(): boolean
	name(): string
	description(): string | undefined
	model: DataModel
	field(fieldName: string): FieldAttributeChain
	getFormattedTypeName(formatter: TypeFormatter): string
}

export interface FieldAttributeChain {
	getString(argName: string): string | undefined
	getBoolean(argName: string): boolean | undefined
	getNumber(argName: string): number | undefined
	attr(attrName: string): boolean
	exists(): boolean
	isIgnored(): boolean
	isSortable(): boolean
	isFilterable(): boolean
	name(): string
	description(): string | undefined
	shouldInclude(includeRelations: boolean): boolean
	isSortableType(): boolean
	isRangeFilterableType(): boolean
	isStringSearchableType(): boolean
	field: DataModelField | undefined
	model: DataModel
	getFormattedFieldName(formatter: TypeFormatter): string
}

export class SchemaProcessor {
	static fromModel(model: DataModel): ModelAttributeChain {
		return new SchemaProcessor().model(model)
	}

	model(model: DataModel): ModelAttributeChain {
		const getAttrValue = <T>(attrName: string, getter: AttributeGetter<T>, argName?: string): T | undefined => {
			const attr = this.findAttribute(model.attributes, attrName)
			return getter(attr, argName)
		}

		return {
			getString: (argName: string): string | undefined => {
				const attrName = argName === 'name' ? '@@graphql.name' : argName === 'description' ? '@@graphql.description' : null
				return attrName ? getAttrValue(attrName, this.getStringValue) : undefined
			},
			getBoolean: (argName: string): boolean | undefined => getAttrValue('@@graphql.connection', this.getBooleanValue, argName),
			getNumber: (argName: string): number | undefined => getAttrValue('@@graphql.connection', this.getNumberValue, argName),
			exists: (): boolean => true,
			isIgnored: (): boolean => this.hasAttribute(model.attributes, '@@graphql.ignore'),
			name: (): string => getAttrValue('@@graphql.name', this.getStringValue) ?? model.name,
			description: (): string | undefined => getAttrValue('@@graphql.description', this.getStringValue),
			model,
			field: (fieldName: string): FieldAttributeChain => this.field(model, fieldName),
			getFormattedTypeName: (formatter: TypeFormatter): string => {
				const customName = getAttrValue('@@graphql.name', this.getStringValue)
				return formatter.formatTypeName(customName ?? model.name)
			},
		}
	}

	field(model: DataModel, fieldName: string): FieldAttributeChain {
		const field = model.fields?.find((f) => f.name === fieldName)

		const getAttrValue = <T>(attrName: string, getter: AttributeGetter<T>, argName?: string): T | undefined => {
			if (!field) return undefined
			const attr = this.findAttribute(field.attributes, attrName)
			return getter(attr, argName)
		}

		const hasAttr = (attrName: string): boolean => (field ? this.hasAttribute(field.attributes, attrName) : false)

		const fieldType = field?.type.type

		return {
			getString: (argName: string): string | undefined => {
				const attrName = argName === 'name' ? '@graphql.name' : argName === 'description' ? '@graphql.description' : null
				return attrName ? getAttrValue(attrName, this.getStringValue) : undefined
			},
			getBoolean: (argName: string): boolean | undefined =>
				field
					? getAttrValue('@graphql.sortable', this.getBooleanValue, argName) || getAttrValue('@graphql.filterable', this.getBooleanValue, argName)
					: undefined,
			getNumber: (argName: string): number | undefined =>
				field
					? getAttrValue('@graphql.sortable', this.getNumberValue, argName) || getAttrValue('@graphql.filterable', this.getNumberValue, argName)
					: undefined,
			attr: (attrName: string): boolean => hasAttr(attrName),
			exists: (): boolean => !!field,
			isIgnored: (): boolean => hasAttr('@graphql.ignore'),
			isSortable: (): boolean => hasAttr('@graphql.sortable'),
			isFilterable: (): boolean => hasAttr('@graphql.filterable'),
			name: (): string => {
				if (!field) return fieldName
				return getAttrValue('@graphql.name', this.getStringValue) ?? fieldName
			},
			description: (): string | undefined => getAttrValue('@graphql.description', this.getStringValue),
			shouldInclude: (includeRelations: boolean): boolean => {
				if (!field) return false

				const isRelation = field.type.reference?.ref?.name && !field.type.type
				if (isRelation && !includeRelations) return false

				return !hasAttr('@graphql.ignore')
			},
			isSortableType: (): boolean => {
				if (!fieldType) return false
				return ['Int', 'Float', 'Decimal', 'DateTime', 'String', 'Boolean'].includes(fieldType)
			},
			isRangeFilterableType: (): boolean => {
				if (!fieldType) return false
				return ['Int', 'Float', 'Decimal', 'DateTime'].includes(fieldType)
			},
			isStringSearchableType: (): boolean => fieldType === 'String',
			field,
			model,
			getFormattedFieldName: (formatter: TypeFormatter): string => {
				if (!field) return formatter.formatFieldName(fieldName)
				const customName = getAttrValue('@graphql.name', this.getStringValue)
				return formatter.formatFieldName(customName ?? field.name)
			},
		}
	}

	private findAttribute(attributes: readonly (DataModelAttribute | DataModelFieldAttribute)[] | undefined, attrName: string): AttributeType {
		return attributes?.find((attr) => attr.decl?.ref?.name === attrName)
	}

	private hasAttribute(attributes: readonly (DataModelAttribute | DataModelFieldAttribute)[] | undefined, attrName: string): boolean {
		return !!this.findAttribute(attributes, attrName)
	}

	private getAttributeArg(attr: AttributeType, argName?: string): AttributeArg | undefined {
		if (!attr?.args?.length) return undefined
		return argName ? attr.args.find((arg) => arg.name === argName) : attr.args[0]
	}

	private getStringValue: AttributeGetter<string> = (attr, argName) => {
		const arg = this.getAttributeArg(attr, argName)
		const exprValue = arg?.value
		return exprValue?.$type === 'StringLiteral' ? (exprValue.value as string) : undefined
	}

	private getBooleanValue: AttributeGetter<boolean> = (attr, argName) => {
		const arg = this.getAttributeArg(attr, argName)
		const exprValue = arg?.value
		return exprValue?.$type === 'BooleanLiteral' ? (exprValue.value as boolean) : undefined
	}

	private getNumberValue: AttributeGetter<number> = (attr, argName) => {
		const arg = this.getAttributeArg(attr, argName)
		const exprValue = arg?.value
		if (exprValue?.$type === 'NumberLiteral') {
			const value = exprValue.value
			return typeof value === 'number' ? value : Number(value)
		}
		return undefined
	}

	enum(enumType: Enum): EnumAttributeChain {
		const getAttrValue = <T>(attrName: string, getter: AttributeGetter<T>, argName?: string): T | undefined => {
			const attr = this.findAttribute(enumType.attributes, attrName)
			return getter(attr, argName)
		}

		return {
			description: (): string | undefined => getAttrValue('@@graphql.description', this.getStringValue),
		}
	}
}

interface EnumAttributeChain {
	description(): string | undefined
}
