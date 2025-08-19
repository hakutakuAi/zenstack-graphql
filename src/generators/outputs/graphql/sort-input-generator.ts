import { BaseGenerator } from '@generators/base-generator'
import { TypeKind } from '@utils/registry'
import { DataModel } from '@zenstackhq/sdk/ast'
import { ErrorCategory, PluginError, handleError } from '@utils/error'

export class SortInputGenerator extends BaseGenerator {
	generate(): string[] {
		this.createSortDirectionEnum()
		this.forEachValidModel((model) => {
			try {
				this.generateSortInputType(model)
			} catch (error) {
				handleError(error, `generate sort input for model ${model.name}`, { model: model.name })
			}
		})
		return this.registry.getTypeNamesByKind(TypeKind.INPUT).filter((name) => name.endsWith('SortInput'))
	}

	private createSortDirectionEnum(): void {
		if (this.registry.isTypeOfKind('SortDirection', TypeKind.ENUM)) {
			return
		}

		try {
			const sortDirectionTC = this.typeFactories.createSortDirectionEnum()
			this.registry.registerType('SortDirection', TypeKind.ENUM, sortDirectionTC, true)
		} catch (error) {
			throw new PluginError(`Failed to create SortDirection enum`, ErrorCategory.GENERATION, { originalError: error }, [
				'Check if SortDirection enum is already defined elsewhere',
			])
		}
	}

	private generateSortInputType(model: DataModel): void {
		const typeName = this.attributeProcessor.model(model).getFormattedTypeName(this.typeFormatter)
		const sortInputName = this.typeFormatter.formatSortInputTypeName(typeName)

		if (this.schemaComposer.has(sortInputName)) {
			return
		}

		const fields = model.fields
			.filter((field) => {
				const fieldProcessor = this.attributeProcessor.field(model, field.name)
				const isAttrSortable = fieldProcessor.isSortable()
				const isTypeSortable = fieldProcessor.isSortableType()
				const shouldInclude = fieldProcessor.shouldInclude(true)
				return isAttrSortable && isTypeSortable && shouldInclude
			})
			.reduce(
				(acc, field) => {
					const fieldName = this.attributeProcessor.field(model, field.name).getFormattedFieldName(this.typeFormatter)
					acc[fieldName] = { description: `Sort by ${fieldName}` }
					return acc
				},
				{} as Record<string, { description: string }>,
			)

		try {
			const sortInputTC = this.typeFactories.createSortInputType(typeName, fields)
			this.registry.registerType(sortInputName, TypeKind.INPUT, sortInputTC, true)
		} catch (error) {
			throw new PluginError(`Error creating sort input type for ${model.name}`, ErrorCategory.GENERATION, { modelName: model.name, error }, [
				'Check if the model fields are defined correctly',
			])
		}
	}
}
