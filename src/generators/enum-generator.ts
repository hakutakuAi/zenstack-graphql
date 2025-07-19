import { EnumTypeComposer } from 'graphql-compose'
import { BaseGenerator } from '@generators/base-generator'
import { GeneratorContext } from '@types'
import type { DMMF } from '@prisma/generator-helper'
import { TypeKind } from '@utils/registry/unified-registry'
import { Generate, SchemaOp } from '@utils/error'
import { Enum } from '@zenstackhq/sdk/ast'

export interface EnumValueConfig {
	value: string
	description?: string
}

export class EnumGenerator extends BaseGenerator {
	private enums: readonly DMMF.DatamodelEnum[] | Enum[]

	constructor(context: GeneratorContext) {
		super(context)
		if (!context.enums) {
			throw new Error('DMMF enums are required for EnumGenerator')
		}
		this.enums = context.enums
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

		this.enums.forEach((enumObj) => this.generateEnum(enumObj))
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
	private generateEnum(enumObj: DMMF.DatamodelEnum | Enum): void {
		const enumName = this.getEnumName(enumObj)

		if (this.hasEnum(enumName)) {
			return
		}

		const enumValues = this.createEnumValues(enumObj)
		const enumDescription = this.getEnumDescription(enumObj)

		const enumComposer = this.schemaComposer.createEnumTC({
			name: enumName,
			description: enumDescription,
			values: enumValues,
		})

		this.schemaComposer.set(enumName, enumComposer)
		this.registry.registerType(enumName, TypeKind.ENUM, enumComposer, true)
	}

	private getEnumName(enumObj: DMMF.DatamodelEnum | Enum): string {
		return this.typeFormatter.formatTypeName(enumObj.name)
	}

	private getEnumDescription(enumObj: DMMF.DatamodelEnum | Enum): string | undefined {
		if ('documentation' in enumObj) {
			return enumObj.documentation || undefined
		}

		if ('comments' in enumObj) {
			return enumObj.comments?.[0]
		}

		return undefined
	}

	private createEnumValues(enumObj: DMMF.DatamodelEnum | Enum): Record<string, EnumValueConfig> {
		const values: Record<string, EnumValueConfig> = {}

		if ('values' in enumObj) {
			for (const enumValue of enumObj.values) {
				const valueName = this.getEnumValueNameFromDMMF(enumValue)

				values[valueName] = {
					value: enumValue.name,
				}
			}
		} else if ('fields' in enumObj) {
			for (const field of enumObj.fields) {
				const valueName = this.getEnumValueNameFromField(field)

				values[valueName] = {
					value: field.name,
				}
			}
		}

		return values
	}

	private getEnumValueNameFromDMMF(enumValue: DMMF.EnumValue): string {
		return this.typeFormatter.formatEnumValueName(enumValue.name)
	}

	private getEnumValueNameFromField(field: any): string {
		return this.typeFormatter.formatEnumValueName(field.name)
	}
}
