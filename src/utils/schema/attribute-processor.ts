import { DataModel, DataModelField } from '@zenstackhq/sdk/ast'
import { HandleErrors } from '@utils/error/error-decorators'

type AttributeArg = {
	name: string
	value: any
}

interface ModelAttributeChain {
	getString(argName: string): string | undefined
	getBoolean(argName: string): boolean | undefined
	getNumber(argName: string): number | undefined
	exists(): boolean
	model(): DataModel
	field(fieldName: string): FieldAttributeChain
}

interface FieldAttributeChain {
	attr(attrName: string): FieldAttributeValueChain
	name(): string
	description(): string | undefined
	exists(): boolean
	isIgnored(): boolean
	isNonSortable(): boolean
	isNonFilterable(): boolean
	inputTypeName(): string | undefined
	field(): DataModelField | undefined
	model(): DataModel
}

interface FieldAttributeValueChain {
	getString(argName: string): string | undefined
	getBoolean(argName: string): boolean | undefined
	getNumber(argName: string): number | undefined
	exists(): boolean
	field(): DataModelField | undefined
	model(): DataModel
}

export class AttributeProcessor {
	processModel(model: DataModel): ModelAttributeChain {
		return {
			getString: (argName: string): string | undefined => this.getModelStringArg(model, '@@graphql.name', argName),
			getBoolean: (argName: string): boolean | undefined => this.getModelBooleanArg(model, '@@graphql', argName),
			getNumber: (argName: string): number | undefined => this.getModelNumberArg(model, '@@graphql', argName),
			exists: (): boolean => true,
			model: (): DataModel => model,
			field: (fieldName: string): FieldAttributeChain => this.processField(model, fieldName),
		}
	}

	attr(model: DataModel, attrName: string): ModelAttributeChain {
		return {
			getString: (argName: string): string | undefined => this.getModelStringArg(model, attrName, argName),
			getBoolean: (argName: string): boolean | undefined => this.getModelBooleanArg(model, attrName, argName),
			getNumber: (argName: string): number | undefined => this.getModelNumberArg(model, attrName, argName),
			exists: (): boolean => this.modelHasAttribute(model, attrName),
			model: (): DataModel => model,
			field: (fieldName: string): FieldAttributeChain => this.processField(model, fieldName),
		}
	}

	processField(model: DataModel, fieldName: string): FieldAttributeChain {
		const field = this.findField(model, fieldName)

		return {
			attr: (attrName: string): FieldAttributeValueChain => ({
				getString: (argName: string): string | undefined => (field ? this.getFieldStringArg(field, attrName, argName) : undefined),
				getBoolean: (argName: string): boolean | undefined => (field ? this.getFieldBooleanArg(field, attrName, argName) : undefined),
				getNumber: (argName: string): number | undefined => (field ? this.getFieldNumberArg(field, attrName, argName) : undefined),
				exists: (): boolean => (field ? this.fieldHasAttribute(field, attrName) : false),
				field: (): DataModelField | undefined => field,
				model: (): DataModel => model,
			}),

			name: (): string => {
				if (!field) return fieldName
				const customName = this.getFieldStringArg(field, '@graphql.name', 'name')
				return customName || fieldName
			},

			description: (): string | undefined => {
				if (!field) return undefined
				return this.getFieldStringArg(field, '@graphql.description', 'description')
			},

			exists: (): boolean => !!field,

			isIgnored: (): boolean => {
				return field ? this.fieldHasAttribute(field, '@graphql.ignore') : false
			},

			isNonSortable: (): boolean => {
				return field ? this.fieldHasAttribute(field, '@graphql.nonSortable') : false
			},

			isNonFilterable: (): boolean => {
				return field ? this.fieldHasAttribute(field, '@graphql.nonFilterable') : false
			},

			inputTypeName: (): string | undefined => {
				if (!field) return undefined
				return this.getFieldStringArg(field, '@graphql.inputType', 'name')
			},

			field: (): DataModelField | undefined => field,

			model: (): DataModel => model,
		}
	}

	getModelName(model: DataModel): string {
		return this.getModelStringArg(model, '@@graphql.name', 'name') || model.name
	}

	getModelDescription(model: DataModel): string | undefined {
		return this.getModelStringArg(model, '@@graphql.description', 'description')
	}

	hasModelIgnoreAttr(model: DataModel): boolean {
		return this.modelHasAttribute(model, '@@graphql.ignore')
	}

	getModelConnectionConfig(model: DataModel): { relay?: boolean; pageSize?: number } {
		const attr = this.findModelAttribute(model, '@@graphql.connection')
		return {
			relay: this.getBooleanAttributeArg(attr, 'relay'),
			pageSize: this.getNumberAttributeArg(attr, 'pageSize'),
		}
	}

	getFieldName(model: DataModel, fieldName: string): string {
		const field = this.findField(model, fieldName)
		if (!field) return fieldName

		return this.getFieldStringArg(field, '@graphql.name', 'name') || fieldName
	}

	getFieldDescription(model: DataModel, fieldName: string): string | undefined {
		const field = this.findField(model, fieldName)
		if (!field) return undefined

		return this.getFieldStringArg(field, '@graphql.description', 'description')
	}

	hasFieldIgnoreAttr(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.fieldHasAttribute(field, '@graphql.ignore') : false
	}

	isFieldNonSortable(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.fieldHasAttribute(field, '@graphql.nonSortable') : false
	}

	isFieldNonFilterable(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.fieldHasAttribute(field, '@graphql.nonFilterable') : false
	}

	getFieldInputTypeName(model: DataModel, fieldName: string): string | undefined {
		const field = this.findField(model, fieldName)
		if (!field) return undefined

		return this.getFieldStringArg(field, '@graphql.inputType', 'name')
	}

	private getCacheKey(prefix: string, model: DataModel, suffix: string = ''): string {
		return `${prefix}:${model.name}:${suffix}`
	}

	@HandleErrors()
	private modelHasAttribute(model: DataModel, attrName: string): boolean {
		return !!model.attributes?.some((attr) => attr.decl.ref?.name === attrName)
	}

	@HandleErrors()
	private fieldHasAttribute(field: DataModelField, attrName: string): boolean {
		return !!field.attributes?.some((attr) => attr.decl.ref?.name === attrName)
	}

	@HandleErrors()
	private findModelAttribute(model: DataModel, attrName: string): any | undefined {
		return model.attributes?.find((attr) => attr.decl.ref?.name === attrName)
	}

	@HandleErrors()
	private findFieldAttribute(field: DataModelField, attrName: string): any | undefined {
		return field.attributes?.find((attr) => attr.decl.ref?.name === attrName)
	}

	@HandleErrors()
	private findField(model: DataModel, fieldName: string): DataModelField | undefined {
		return model.fields?.find((f) => f.name === fieldName)
	}

	@HandleErrors()
	private getAttributeArg(attr: any, argName: string): AttributeArg | undefined {
		if (!attr?.args?.length) return undefined
		return attr.args.find((arg: any) => arg.name === argName)
	}

	@HandleErrors()
	private getStringAttributeArg(attr: any, argName: string): string | undefined {
		const arg = this.getAttributeArg(attr, argName)
		return arg && typeof arg.value === 'string' ? arg.value : undefined
	}

	@HandleErrors()
	private getBooleanAttributeArg(attr: any, argName: string): boolean | undefined {
		const arg = this.getAttributeArg(attr, argName)
		return arg && typeof arg.value === 'boolean' ? arg.value : undefined
	}

	@HandleErrors()
	private getNumberAttributeArg(attr: any, argName: string): number | undefined {
		const arg = this.getAttributeArg(attr, argName)
		return arg && typeof arg.value === 'number' ? arg.value : undefined
	}

	@HandleErrors()
	private getModelStringArg(model: DataModel, attrName: string, argName: string): string | undefined {
		return this.getStringAttributeArg(this.findModelAttribute(model, attrName), argName)
	}

	@HandleErrors()
	private getModelBooleanArg(model: DataModel, attrName: string, argName: string): boolean | undefined {
		return this.getBooleanAttributeArg(this.findModelAttribute(model, attrName), argName)
	}

	@HandleErrors()
	private getModelNumberArg(model: DataModel, attrName: string, argName: string): number | undefined {
		return this.getNumberAttributeArg(this.findModelAttribute(model, attrName), argName)
	}

	@HandleErrors()
	private getFieldStringArg(field: DataModelField, attrName: string, argName: string): string | undefined {
		return this.getStringAttributeArg(this.findFieldAttribute(field, attrName), argName)
	}

	@HandleErrors()
	private getFieldBooleanArg(field: DataModelField, attrName: string, argName: string): boolean | undefined {
		return this.getBooleanAttributeArg(this.findFieldAttribute(field, attrName), argName)
	}

	@HandleErrors()
	private getFieldNumberArg(field: DataModelField, attrName: string, argName: string): number | undefined {
		return this.getNumberAttributeArg(this.findFieldAttribute(field, attrName), argName)
	}
}
