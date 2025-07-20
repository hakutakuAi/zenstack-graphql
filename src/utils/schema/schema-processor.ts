import { AttributeArg, DataModel, DataModelField, DataModelAttribute, DataModelFieldAttribute } from '@zenstackhq/sdk/ast'

type ExpressionValue = { $type: string; value: any }

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
}

export class SchemaProcessor {
	static fromModel(model: DataModel): ModelAttributeChain {
		return new SchemaProcessor().model(model)
	}

	model(model: DataModel): ModelAttributeChain {
		return {
			getString: (argName: string): string | undefined => {
				if (argName === 'name') {
					return this.getModelStringArg(model, '@@graphql.name', 'name')
				} else if (argName === 'description') {
					return this.getModelStringArg(model, '@@graphql.description', 'description')
				}
				return undefined
			},

			getBoolean: (argName: string): boolean | undefined => this.getModelBooleanArg(model, '@@graphql.connection', argName),

			getNumber: (argName: string): number | undefined => this.getModelNumberArg(model, '@@graphql.connection', argName),

			exists: (): boolean => true,

			isIgnored: (): boolean => this.modelHasAttribute(model, '@@graphql.ignore'),

			name: (): string => {
				const customName = this.getModelStringArg(model, '@@graphql.name', 'name')
				return customName !== undefined ? customName : model.name
			},

			description: (): string | undefined => {
				return this.getModelStringArg(model, '@@graphql.description', 'description')
			},

			model,

			field: (fieldName: string): FieldAttributeChain => this.field(model, fieldName),
		}
	}

	field(model: DataModel, fieldName: string): FieldAttributeChain {
		const field = this.findField(model, fieldName)

		return {
			getString: (argName: string): string | undefined => {
				if (!field) return undefined

				if (argName === 'name') {
					return this.getFieldStringArg(field, '@graphql.name', 'name')
				} else if (argName === 'description') {
					return this.getFieldStringArg(field, '@graphql.description', 'description')
				}
				return undefined
			},

			getBoolean: (argName: string): boolean | undefined =>
				field ? this.getFieldBooleanArg(field, '@graphql.sortable', argName) || this.getFieldBooleanArg(field, '@graphql.filterable', argName) : undefined,

			getNumber: (argName: string): number | undefined =>
				field ? this.getFieldNumberArg(field, '@graphql.sortable', argName) || this.getFieldNumberArg(field, '@graphql.filterable', argName) : undefined,

			attr: (attrName: string): boolean => (field ? this.fieldHasAttribute(field, attrName) : false),

			exists: (): boolean => !!field,

			isIgnored: (): boolean => (field ? this.fieldHasAttribute(field, '@graphql.ignore') : false),

			isSortable: (): boolean => (field ? this.fieldHasAttribute(field, '@graphql.sortable') : false),

			isFilterable: (): boolean => (field ? this.fieldHasAttribute(field, '@graphql.filterable') : false),

			name: (): string => {
				if (!field) return fieldName

				const customName = this.getFieldStringArg(field, '@graphql.name', 'name')

				return customName !== undefined && customName !== null ? customName : fieldName
			},

			description: (): string | undefined => {
				if (!field) return undefined

				const description = this.getFieldStringArg(field, '@graphql.description', 'description')

				return description
			},

			shouldInclude: (includeRelations: boolean): boolean => {
				if (!field) return false

				const isRelation = field.type.reference && field.type.reference.ref?.name && !field.type.type

				if (isRelation && !includeRelations) {
					return false
				}

				return !this.fieldHasAttribute(field, '@graphql.ignore')
			},

			isSortableType: (): boolean => {
				if (!field || !field.type.type) return false

				switch (field.type.type) {
					case 'Int':
					case 'Float':
					case 'Decimal':
					case 'DateTime':
					case 'String':
					case 'Boolean':
						return true
					default:
						return false
				}
			},

			isRangeFilterableType: (): boolean => {
				if (!field || !field.type.type) return false

				switch (field.type.type) {
					case 'Int':
					case 'Float':
					case 'Decimal':
					case 'DateTime':
						return true
					default:
						return false
				}
			},

			isStringSearchableType: (): boolean => {
				return field?.type.type === 'String' || false
			},

			field,
			model,
		}
	}

	private modelHasAttribute(model: DataModel, attrName: string): boolean {
		if (!model.attributes || model.attributes.length === 0) return false
		return model.attributes.some((attr) => {
			return attr.decl && attr.decl.ref && attr.decl.ref.name === attrName
		})
	}

	private fieldHasAttribute(field: DataModelField, attrName: string): boolean {
		if (!field.attributes || field.attributes.length === 0) return false
		return field.attributes.some((attr) => {
			return attr.decl && attr.decl.ref && attr.decl.ref.name === attrName
		})
	}

	private findModelAttribute(model: DataModel, attrName: string): DataModelAttribute | undefined {
		if (!model.attributes || model.attributes.length === 0) {
			return undefined
		}

		return model.attributes.find(attr => attr.decl?.ref?.name === attrName)
	}

	private findFieldAttribute(field: DataModelField, attrName: string): DataModelFieldAttribute | undefined {
		if (!field.attributes || field.attributes.length === 0) {
			return undefined
		}

		return field.attributes.find(attr => attr.decl?.ref?.name === attrName)
	}

	private findField(model: DataModel, fieldName: string): DataModelField | undefined {
		return model.fields?.find((f) => f.name === fieldName)
	}

	private getAttributeArg(attr: DataModelAttribute | DataModelFieldAttribute | undefined, argName: string): AttributeArg | undefined {
		if (!attr?.args?.length) {
			return undefined
		}

		return attr.args.find(arg => arg.name === argName)
	}

	private getStringAttributeArg(attr: DataModelAttribute | DataModelFieldAttribute | undefined, argName: string): string | undefined {
		const arg = this.getAttributeArg(attr, argName)
		if (!arg?.value) {
			return undefined
		}

		const exprValue = arg.value as ExpressionValue
		if (exprValue.$type === 'StringLiteral') {
			return exprValue.value as string
		}

		return undefined
	}

	private getBooleanAttributeArg(attr: DataModelAttribute | DataModelFieldAttribute | undefined, argName: string): boolean | undefined {
		const arg = this.getAttributeArg(attr, argName)
		if (!arg?.value) return undefined

		const exprValue = arg.value as ExpressionValue
		if (exprValue.$type === 'BooleanLiteral') {
			return exprValue.value as boolean
		}

		return undefined
	}

	private getNumberAttributeArg(attr: DataModelAttribute | DataModelFieldAttribute | undefined, argName: string): number | undefined {
		const arg = this.getAttributeArg(attr, argName)
		if (!arg?.value) return undefined

		const exprValue = arg.value as ExpressionValue
		if (exprValue.$type === 'NumberLiteral') {
			const value = exprValue.value
			return typeof value === 'number' ? value : Number(value)
		}

		return undefined
	}

	private getModelStringArg(model: DataModel, attrName: string, argName: string): string | undefined {
		return this.getStringAttributeArg(this.findModelAttribute(model, attrName), argName)
	}

	private getModelBooleanArg(model: DataModel, attrName: string, argName: string): boolean | undefined {
		return this.getBooleanAttributeArg(this.findModelAttribute(model, attrName), argName)
	}

	private getModelNumberArg(model: DataModel, attrName: string, argName: string): number | undefined {
		return this.getNumberAttributeArg(this.findModelAttribute(model, attrName), argName)
	}

	private getFieldStringArg(field: DataModelField, attrName: string, argName: string): string | undefined {
		return this.getStringAttributeArg(this.findFieldAttribute(field, attrName), argName)
	}

	private getFieldBooleanArg(field: DataModelField, attrName: string, argName: string): boolean | undefined {
		return this.getBooleanAttributeArg(this.findFieldAttribute(field, attrName), argName)
	}

	private getFieldNumberArg(field: DataModelField, attrName: string, argName: string): number | undefined {
		return this.getNumberAttributeArg(this.findFieldAttribute(field, attrName), argName)
	}
}
