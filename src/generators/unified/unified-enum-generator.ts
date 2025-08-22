import { BaseGeneratorContext } from '@core/types'
import { GraphQLRegistry } from '@utils/registry'
import { SchemaComposer } from 'graphql-compose'
import { TypeKind } from '@utils/registry/base-registry'
import { Enum } from '@zenstackhq/sdk/ast'
import { createGenerationContext, executeSafely } from '@utils/error'
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

export class UnifiedEnumGenerator {
	private astFactory?: TypeScriptASTFactory
	private format: OutputFormat
	private typeFormatterOverride?: TypeFormatter
	private context: BaseGeneratorContext
	private typeFormatter: TypeFormatter
	private registry?: GraphQLRegistry
	private schemaComposer?: SchemaComposer<unknown>

	constructor(context: BaseGeneratorContext, format: OutputFormat = OutputFormat.GRAPHQL, typeFormatter?: TypeFormatter) {
		this.context = context
		this.format = format
		this.typeFormatterOverride = typeFormatter
		this.typeFormatter = new TypeFormatter(context.options.typeNaming, context.options.fieldNaming)

		if (format === OutputFormat.GRAPHQL && 'registry' in context) {
			this.registry = (context as any).registry
			this.schemaComposer = (context as any).registry?.schemaComposer
		}

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
		return this.processEnums((enumObj: Enum) => this.generateGraphQLEnum(enumObj))
	}

	private generateTypeScriptEnums(): string[] {
		return this.processEnums((enumType: Enum) => this.generateTypeScriptEnum(enumType))
	}

	private processEnums(processor: (enumObj: Enum) => any): string[] {
		const results: string[] = []

		for (const enumObj of this.context.enums) {
			try {
				const result = processor(enumObj)
				if (result?.success && result?.result) {
					results.push(result.result)
				}
			} catch (error) {
				console.warn(`Failed to process enum ${enumObj.name}:`, error)
			}
		}

		return results
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

				if (!this.schemaComposer) {
					throw new Error('SchemaComposer not initialized')
				}

				const enumComposer = this.schemaComposer.createEnumTC({
					name: enumName,
					description: enumDescription,
					values: enumValues,
				})

				this.registry?.registerType(enumName, TypeKind.ENUM, enumComposer, true)
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

	private getFormattedName(name: string): string {
		const formatter = this.typeFormatterOverride || this.typeFormatter
		return formatter.formatTypeName(name)
	}

	private hasEnum(name: string): boolean {
		return this.registry?.isTypeOfKind(name, TypeKind.ENUM) || false
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
