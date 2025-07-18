import type { DataModel, DataModelField } from '@zenstackhq/sdk/ast'

type AttributeArg = {
	name: string
	value: any
}

export class AttributeProcessor {
	getModelName(model: DataModel): string {
		return this.getStringArg(this.findModelAttr(model, '@@graphql.name'), 'name') || model.name
	}

	getModelDescription(model: DataModel): string | undefined {
		return this.getStringArg(this.findModelAttr(model, '@@graphql.description'), 'description')
	}

	hasModelIgnoreAttr(model: DataModel): boolean {
		return this.hasModelAttr(model, '@@graphql.ignore')
	}

	getModelConnectionConfig(model: DataModel): { relay?: boolean; pageSize?: number } {
		const attr = this.findModelAttr(model, '@@graphql.connection')
		return {
			relay: this.getBooleanArg(attr, 'relay'),
			pageSize: this.getNumberArg(attr, 'pageSize'),
		}
	}

	getFieldName(model: DataModel, fieldName: string): string {
		const field = this.findField(model, fieldName)
		if (!field) return fieldName

		return this.getStringArg(this.findFieldAttr(field, '@graphql.name'), 'name') || fieldName
	}

	getFieldDescription(model: DataModel, fieldName: string): string | undefined {
		const field = this.findField(model, fieldName)
		if (!field) return undefined

		return this.getStringArg(this.findFieldAttr(field, '@graphql.description'), 'description')
	}

	hasFieldIgnoreAttr(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.hasFieldAttr(field, '@graphql.ignore') : false
	}

	isFieldNonSortable(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.hasFieldAttr(field, '@graphql.nonSortable') : false
	}

	isFieldNonFilterable(model: DataModel, fieldName: string): boolean {
		const field = this.findField(model, fieldName)
		return field ? this.hasFieldAttr(field, '@graphql.nonFilterable') : false
	}

	getFieldInputTypeName(model: DataModel, fieldName: string): string | undefined {
		const field = this.findField(model, fieldName)
		if (!field) return undefined

		return this.getStringArg(this.findFieldAttr(field, '@graphql.inputType'), 'name')
	}

	private hasModelAttr(model: DataModel, attrName: string): boolean {
		return model.attributes?.some((attr) => attr.decl.ref?.name === attrName) || false
	}

	private hasFieldAttr(field: DataModelField, attrName: string): boolean {
		return field.attributes?.some((attr) => attr.decl.ref?.name === attrName) || false
	}

	private findModelAttr(model: DataModel, attrName: string): any | undefined {
		return model.attributes?.find((attr) => attr.decl.ref?.name === attrName)
	}

	private findFieldAttr(field: DataModelField, attrName: string): any | undefined {
		return field.attributes?.find((attr) => attr.decl.ref?.name === attrName)
	}

	private findField(model: DataModel, fieldName: string): DataModelField | undefined {
		return model.fields?.find((f) => f.name === fieldName)
	}

	private getArg(attr: any, argName: string): AttributeArg | undefined {
		if (!attr?.args?.length) return undefined
		return attr.args.find((arg: any) => arg.name === argName)
	}

	private getStringArg(attr: any, argName: string): string | undefined {
		const arg = this.getArg(attr, argName)
		return arg && typeof arg.value === 'string' ? arg.value : undefined
	}

	private getBooleanArg(attr: any, argName: string): boolean | undefined {
		const arg = this.getArg(attr, argName)
		return arg && typeof arg.value === 'boolean' ? arg.value : undefined
	}

	private getNumberArg(attr: any, argName: string): number | undefined {
		const arg = this.getArg(attr, argName)
		return arg && typeof arg.value === 'number' ? arg.value : undefined
	}
}
