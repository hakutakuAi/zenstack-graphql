import { AbstractGenerator } from '@generators/abstract-generator'
import { TypeKind } from '@utils/registry/base-registry'
import { Enum } from '@zenstackhq/sdk/ast'
import { createGenerationContext, executeSafely } from '@utils/generator-utils'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { OutputFormat } from '@utils/constants'
import { TypeFormatter } from '@utils/schema/type-formatter'

export interface EnumValueConfig {
	value: string
	description?: string
}

export interface EnumGenerationResult {
	graphqlTypes: string[]
	typescriptTypes: string[]
}

export class UnifiedEnumGenerator extends AbstractGenerator<EnumGenerationResult> {
	private astFactory?: TypeScriptASTFactory
	private format: OutputFormat
	private typeFormatterOverride?: TypeFormatter

	constructor(context: any, format: OutputFormat = OutputFormat.GRAPHQL, typeFormatter?: TypeFormatter) {
		super(context)
		this.format = format
		this.typeFormatterOverride = typeFormatter

		if (format === OutputFormat.TYPE_GRAPHQL) {
			const formatter = this.typeFormatterOverride || this.typeFormatter
			if (!formatter) {
				throw new Error('TypeFormatter is required for TypeScript enum generation')
			}
			this.astFactory = new TypeScriptASTFactory(formatter)
		}
	}

	generate(): EnumGenerationResult {
		const result: EnumGenerationResult = {
			graphqlTypes: [],
			typescriptTypes: [],
		}

		if (this.format === OutputFormat.GRAPHQL) {
			result.graphqlTypes = this.generateGraphQLEnums()
		} else {
			result.typescriptTypes = this.generateTypeScriptEnums()
		}

		return result
	}

	private generateGraphQLEnums(): string[] {
		return this.processEnums((enumObj) => this.generateGraphQLEnum(enumObj))
	}

	private generateTypeScriptEnums(): string[] {
		return this.processEnums((enumType) => this.generateTypeScriptEnum(enumType))
	}

	private generateGraphQLEnum(enumObj: Enum) {
		const enumName = this.getFormattedName(enumObj.name)

		if (this.hasEnum(enumName)) {
			return { success: true, result: enumName }
		}

		return executeSafely(
			() => {
				const enumValues = this.createEnumValues(enumObj)
				const enumDescription = this.getEnumDescription(enumObj)

				const enumComposer = this.schemaComposer.createEnumTC({
					name: enumName,
					description: enumDescription,
					values: enumValues,
				})

				this.registry.registerType(enumName, TypeKind.ENUM, enumComposer, true)
				return enumName
			},
			createGenerationContext(enumName, 'enum', 'generate'),
		)
	}

	private generateTypeScriptEnum(enumType: Enum) {
		return executeSafely(
			() => {
				if (!this.astFactory) {
					throw new Error('AST factory not initialized for TypeScript generation')
				}
				const enumDeclaration = this.astFactory.createEnumType(enumType)
				return enumDeclaration.getFullText()
			},
			createGenerationContext(enumType.name, 'TypeScript enum', 'generate'),
		)
	}

	private getEnumDescription(enumObj: Enum): string | undefined {
		return enumObj.comments?.[0]
	}

	private createEnumValues(enumObj: Enum): Record<string, EnumValueConfig> {
		const values: Record<string, EnumValueConfig> = {}

		for (const field of enumObj.fields) {
			const valueName = this.getEnumValueNameFromField(field)

			values[valueName] = {
				value: field.name,
			}
		}

		return values
	}

	private getEnumValueNameFromField(field: any): string {
		const formatter = this.typeFormatterOverride || this.typeFormatter
		return formatter.formatEnumValueName(field.name)
	}

	protected override getFormattedName(name: string): string {
		const formatter = this.typeFormatterOverride || this.typeFormatter
		return formatter.formatTypeName(name)
	}

	hasEnum(name: string): boolean {
		return this.registry.isTypeOfKind(name, TypeKind.ENUM)
	}

	static getSupportedFormats(): OutputFormat[] {
		return [OutputFormat.GRAPHQL, OutputFormat.TYPE_GRAPHQL]
	}

	static createGraphQLGenerator(context: any): UnifiedEnumGenerator {
		return new UnifiedEnumGenerator(context, OutputFormat.GRAPHQL)
	}

	static createTypeScriptGenerator(context: any, typeFormatter?: TypeFormatter): UnifiedEnumGenerator {
		return new UnifiedEnumGenerator(context, OutputFormat.TYPE_GRAPHQL, typeFormatter)
	}
}

export function createGraphQLEnumGenerator(context: any): UnifiedEnumGenerator {
	return UnifiedEnumGenerator.createGraphQLGenerator(context)
}

export function createTypeScriptEnumGenerator(context: any, typeFormatter?: TypeFormatter): UnifiedEnumGenerator {
	return UnifiedEnumGenerator.createTypeScriptGenerator(context, typeFormatter)
}
