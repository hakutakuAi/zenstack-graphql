import type { DataModel, DataModelField } from '@zenstackhq/sdk/ast'

type AttributeArg = {
	name: string
	value: any
}

export class AttributeProcessor {
	static getModelName(model: DataModel): string {
		return this.getStringArg(this.findModelAttr(model, '@@graphql.name'), 'name') || model.name
	}

	static getModelDescription(model: DataModel): string | undefined {
		return this.getStringArg(this.findModelAttr(model, '@@graphql.description'), 'description')
	}

	static hasModelIgnoreAttr(model: DataModel): boolean {
		return this.hasModelAttr(model, '@@graphql.ignore')
	}

	static getModelConnectionConfig(model: DataModel): { relay?: boolean; pageSize?: number } {
		const attr = this.findModelAttr(model, '@@graphql.connection')
		return {
			relay: this.getBooleanArg(attr, 'relay'),
			pageSize: this.getNumberArg(attr, 'pageSize'),
		}
	}

	static getFieldName(model: DataModel, fieldName: string): string {
		const field = this.findField(model, fieldName)
		if (!field) return fieldName

		return this.getStringArg(this.findFieldAttr(field, '@graphql.name'), 'name') || fieldName
	}

	static getFieldDescription(model: DataModel, fieldName: string): string | undefined {
		const field = this.findField(model, fieldName)
		if (!field) return undefined

		return this.getStringArg(this.findFieldAttr(field, '@graphql.description'), 'description')
	}

	static hasFieldIgnoreAttr(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.hasFieldAttr(field, '@graphql.ignore') : false
	}

	static isFieldNonSortable(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.hasFieldAttr(field, '@graphql.nonSortable') : false
	}

	static isFieldNonFilterable(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.hasFieldAttr(field, '@graphql.nonFilterable') : false
	}

	static getFieldInputTypeName(model: DataModel, fieldName: string): string | undefined {
		const field = this.findField(model, fieldName)
		if (!field) return undefined

		return this.getStringArg(this.findFieldAttr(field, '@graphql.inputType'), 'name')
	}

	private static hasModelAttr(model: DataModel, attrName: string): boolean {
		return model.attributes?.some((attr) => attr.decl.ref?.name === attrName) || false
	}

	private static hasFieldAttr(field: DataModelField, attrName: string): boolean {
		return field.attributes?.some((attr) => attr.decl.ref?.name === attrName) || false
	}

	private static findModelAttr(model: DataModel, attrName: string): any | undefined {
		return model.attributes?.find((attr) => attr.decl.ref?.name === attrName)
	}

	private static findFieldAttr(field: DataModelField, attrName: string): any | undefined {
		return field.attributes?.find((attr) => attr.decl.ref?.name === attrName)
	}

	private static findField(model: DataModel, fieldName: string): DataModelField | undefined {
		return model.fields?.find((f) => f.name === fieldName)
	}

	private static getArg(attr: any, argName: string): AttributeArg | undefined {
		if (!attr?.args?.length) return undefined
		return attr.args.find((arg: any) => arg.name === argName)
	}

	private static getStringArg(attr: any, argName: string): string | undefined {
		const arg = this.getArg(attr, argName)
		return arg && typeof arg.value === 'string' ? arg.value : undefined
	}

	private static getBooleanArg(attr: any, argName: string): boolean | undefined {
		const arg = this.getArg(attr, argName)
		return arg && typeof arg.value === 'boolean' ? arg.value : undefined
	}

	private static getNumberArg(attr: any, argName: string): number | undefined {
		const arg = this.getArg(attr, argName)
		return arg && typeof arg.value === 'number' ? arg.value : undefined
	}
}
