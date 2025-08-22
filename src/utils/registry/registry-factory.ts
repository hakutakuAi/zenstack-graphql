import { SchemaComposer } from 'graphql-compose'
import { GraphQLRegistry } from './graphql-registry'
import { TypeScriptRegistry } from './typescript-registry'
import { BaseRegistry, TypeKind, BaseTypeInfo } from './base-registry'

export type RegistryFormat = 'graphql' | 'type-graphql'

export interface RegistryFactoryOptions {
	format: RegistryFormat
	schemaComposer?: SchemaComposer<unknown>
}

export class RegistryFactory {
	static createRegistry(options: RegistryFactoryOptions): BaseRegistry<any, any> {
		switch (options.format) {
			case 'graphql':
				if (!options.schemaComposer) {
					throw new Error('SchemaComposer is required for GraphQL registry')
				}
				return new GraphQLRegistry(options.schemaComposer)

			case 'type-graphql':
				return new TypeScriptRegistry()

			default:
				throw new Error(`Unsupported registry format: ${options.format}`)
		}
	}

	static createGraphQLRegistry(schemaComposer: SchemaComposer<unknown>): GraphQLRegistry {
		return new GraphQLRegistry(schemaComposer)
	}

	static createTypeScriptRegistry(): TypeScriptRegistry {
		return new TypeScriptRegistry()
	}

	static getSupportedFormats(): RegistryFormat[] {
		return ['graphql', 'type-graphql']
	}

	static isFormatSupported(format: string): format is RegistryFormat {
		return this.getSupportedFormats().includes(format as RegistryFormat)
	}
}

export abstract class EnhancedBaseRegistry<TData, TInfo extends BaseTypeInfo<TData>> extends BaseRegistry<TData, TInfo> {
	getTypesByKindGrouped(): Record<string, TInfo[]> {
		const grouped: Record<string, TInfo[]> = {}

		for (const typeInfo of this.getAllTypes()) {
			const kind = typeInfo.kind
			if (!grouped[kind]) {
				grouped[kind] = []
			}
			grouped[kind].push(typeInfo)
		}

		return grouped
	}

	getTypeStatistics(): Record<string, number> {
		const stats: Record<string, number> = {}

		for (const kind of Object.values(TypeKind)) {
			stats[kind] = this.getTypesByKind(kind).length
		}

		return stats
	}

	hasAnyTypes(): boolean {
		return this.types.size > 0
	}

	getTotalTypeCount(): number {
		return this.types.size
	}

	clearAllTypes(): void {
		this.types.clear()
		this.processedItems.clear()
	}

	bulkRegisterTypes(
		registrations: Array<{
			name: string
			kind: TypeKind
			data: TData
			isGenerated?: boolean
		}>,
	): void {
		for (const registration of registrations) {
			this.registerType(registration.name, registration.kind, registration.data, registration.isGenerated ?? true)
		}
	}

	findTypesByPattern(pattern: RegExp): TInfo[] {
		return this.getAllTypes().filter((typeInfo) => pattern.test(typeInfo.name))
	}

	getDependencyChain(typeName: string): string[] {
		const chain: string[] = []
		const visited = new Set<string>()

		const collectDependencies = (name: string) => {
			if (visited.has(name)) return
			visited.add(name)

			const typeInfo = this.getType(name)
			if (typeInfo && typeInfo.dependencies) {
				for (const dep of typeInfo.dependencies) {
					collectDependencies(dep)
					if (!chain.includes(dep)) {
						chain.push(dep)
					}
				}
			}
		}

		collectDependencies(typeName)
		return chain
	}
}
