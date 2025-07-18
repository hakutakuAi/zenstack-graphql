import { EnumTypeComposer, SchemaComposer } from 'graphql-compose'
import { BaseGenerator } from '@generators/base-generator'
import { GeneratorContext, DMMF } from '@types'
import { ValidationUtils } from '@utils/schema/validation'
import { TypeKind } from '@utils/registry/unified-registry'
import { Generate, SchemaOp, Validate } from '@utils/error'

export interface EnumValueConfig {
	value: string
	description?: string
}

export class EnumGenerator extends BaseGenerator {
	private readonly dmmfEnums: readonly DMMF.DatamodelEnum[]

	constructor(context: GeneratorContext) {
		super(context)
		if (!context.dmmfEnums) {
			throw new Error('DMMF enums are required for EnumGenerator')
		}
		this.dmmfEnums = context.dmmfEnums
	}

	protected override skipGeneration(): boolean {
		return !this.options.generateEnums
	}

	@Generate({
		suggestions: ['Check enum definitions in your schema', 'Ensure enum values are valid GraphQL identifiers', 'Verify enum attributes are properly configured'],
	})
	generate(): void {
		if (this.skipGeneration()) {
			return
		}

		this.dmmfEnums.forEach((dmmfEnum) => this.generateEnum(dmmfEnum))
	}

	getGeneratedEnums(): string[] {
		return this.registry.getEnumTypes()
	}

	hasEnum(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.ENUM)
	}

	getEnumComposer(name: string): EnumTypeComposer | undefined {
		return this.registry.getEnumComposer(name)
	}

	getEnumValues(enumName: string): string[] {
		return this.registry.getEnumValues(enumName)
	}

	isValidEnumValue(enumName: string, value: string): boolean {
		return this.registry.isValidEnumValue(enumName, value)
	}

	@SchemaOp({
		suggestions: ['Check enum definition for name and configuration', 'Ensure enum name is a valid GraphQL identifier', 'Verify enum values are properly defined'],
	})
	private generateEnum(dmmfEnum: DMMF.DatamodelEnum): void {
		const enumName = this.getEnumName(dmmfEnum)

		if (this.hasEnum(enumName)) {
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
		this.registry.registerType(enumName, TypeKind.ENUM, enumComposer, true)
	}

	private getEnumName(dmmfEnum: DMMF.DatamodelEnum): string {
		return this.typeFormatter.formatTypeName(dmmfEnum.name)
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
		return this.typeFormatter.formatEnumValueName(enumValue.name)
	}
}
