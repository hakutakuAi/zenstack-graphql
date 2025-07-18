import { SchemaComposer, EnumTypeComposer } from 'graphql-compose'
import type { DMMF } from '@zenstackhq/sdk/prisma'
import { BaseGenerator } from '@generators/base-generator'
import { AttributeProcessor } from '@utils/attribute-processor'
import { ErrorHandler } from '@utils/error-handler'
import { NormalizedOptions } from '@utils/options-validator'
import { formatEnumValueName } from '@utils/string-utils'

export interface EnumGeneratorContext {
	schemaComposer: SchemaComposer
	options: NormalizedOptions
	errorHandler: ErrorHandler
	attributeProcessor: AttributeProcessor
	dmmfEnums: readonly DMMF.DatamodelEnum[]
}

export interface EnumValueConfig {
	value: string
	description?: string
}

export class EnumGenerator extends BaseGenerator<EnumTypeComposer<any>> {
	private readonly dmmfEnums: readonly DMMF.DatamodelEnum[]

	constructor(context: EnumGeneratorContext) {
		super(context.schemaComposer, context.options, context.errorHandler, context.attributeProcessor)
		this.dmmfEnums = context.dmmfEnums
	}

	generate(): void {
		if (!this.options.generateEnums) {
			return
		}

		try {
			for (const dmmfEnum of this.dmmfEnums) {
				this.generateEnum(dmmfEnum)
			}
		} catch (error) {
			this.handleError('generate', error, ['Check enum definitions in your schema', 'Ensure enum values are valid GraphQL identifiers', 'Verify enum attributes are properly configured'])
		}
	}

	getGeneratedEnums(): string[] {
		return this.getGeneratedItems()
	}

	hasEnum(name: string): boolean {
		return this.hasItem(name)
	}

	getEnumComposer(name: string): EnumTypeComposer | undefined {
		if (!this.schemaComposer.has(name)) {
			return undefined
		}

		const composer = this.schemaComposer.get(name)
		return composer instanceof EnumTypeComposer ? composer : undefined
	}

	getEnumValues(enumName: string): string[] {
		const enumComposer = this.getEnumComposer(enumName)
		return enumComposer ? Object.keys(enumComposer.getFields()) : []
	}

	isValidEnumValue(enumName: string, value: string): boolean {
		return this.getEnumValues(enumName).includes(value)
	}

	private generateEnum(dmmfEnum: DMMF.DatamodelEnum): void {
		try {
			const enumName = this.getEnumName(dmmfEnum)

			if (this.hasItem(enumName)) {
				return
			}

			const enumValues = this.createEnumValues(dmmfEnum)
			const enumDescription = this.getEnumDescription(dmmfEnum)

			const enumComposer = this.schemaComposer.createEnumTC({
				name: enumName,
				description: enumDescription,
				values: enumValues,
			})

			this.schemaComposer.set(enumName, enumComposer)
			this.registerItem(enumName)
		} catch (error) {
			this.handleError('generateEnum', error, [`Check enum definition for "${dmmfEnum.name}"`, 'Ensure enum name is a valid GraphQL identifier', 'Verify enum values are properly defined'])
		}
	}

	private getEnumName(dmmfEnum: DMMF.DatamodelEnum): string {
		return this.formatTypeName(dmmfEnum.name)
	}

	private getEnumDescription(dmmfEnum: DMMF.DatamodelEnum): string | undefined {
		return dmmfEnum.documentation
	}

	private createEnumValues(dmmfEnum: DMMF.DatamodelEnum): Record<string, EnumValueConfig> {
		const values: Record<string, EnumValueConfig> = {}

		for (const enumValue of dmmfEnum.values) {
			const valueName = this.getEnumValueName(enumValue)

			values[valueName] = {
				value: enumValue.name,
			}
		}

		return values
	}

	private getEnumValueName(enumValue: DMMF.EnumValue): string {
		return formatEnumValueName(enumValue.name)
	}
}
