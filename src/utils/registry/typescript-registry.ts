import { TypeKind, BaseTypeInfo } from './base-registry'
import { BaseRegistry } from './base-registry'

export interface TypeScriptTypeInfo extends BaseTypeInfo<string> {
	code: string
	dependencies: string[]
}

export class TypeScriptRegistry extends BaseRegistry<string, TypeScriptTypeInfo> {
	protected createTypeInfo(name: string, kind: TypeKind, code: string, isGenerated: boolean): TypeScriptTypeInfo {
		return {
			name,
			kind,
			data: code,
			code,
			dependencies: [],
			isGenerated,
		}
	}

	registerTypeWithDeps(name: string, kind: TypeKind, code: string, dependencies: string[] = [], isGenerated = true): void {
		if (this.types.has(name)) {
			const existing = this.types.get(name)!
			if (existing.kind !== kind) {
				super.registerType(name, kind, code, isGenerated)
				return
			}
			return
		}

		const typeInfo: TypeScriptTypeInfo = {
			name,
			kind,
			data: code,
			code,
			dependencies,
			isGenerated,
		}

		this.types.set(name, typeInfo)
	}

	validateSchema(): string[] {
		const warnings: string[] = []

		for (const typeInfo of this.getAllTypes()) {
			for (const dependency of typeInfo.dependencies) {
				if (!this.hasType(dependency)) {
					warnings.push(`Type ${typeInfo.name} depends on ${dependency} which is not registered`)
				}
			}
		}

		return warnings
	}

	getObjectTypes(): string[] {
		return this.getTypesByKind(TypeKind.OBJECT).map((info) => info.code)
	}

	getEnumTypes(): string[] {
		return this.getTypesByKind(TypeKind.ENUM).map((info) => info.code)
	}

	getScalarTypes(): string[] {
		return this.getTypesByKind(TypeKind.SCALAR).map((info) => info.code)
	}

	getInputTypes(): string[] {
		return this.getTypesByKind(TypeKind.INPUT).map((info) => info.code)
	}

	getResolverTypes(): string[] {
		return this.getTypesByKind(TypeKind.RESOLVER).map((info) => info.code)
	}

	generateImports(): string[] {
		const imports = [
			'import { ObjectType, Field, ID, Int, Float, registerEnumType, InputType, ArgsType } from "type-graphql"',
			'import { GraphQLJSON } from "graphql-scalars"',
			'import "reflect-metadata"',
		]

		const hasCustomScalars = this.getTypesByKind(TypeKind.SCALAR).length > 0
		if (hasCustomScalars) {
			const scalarTypes = this.getTypesByKind(TypeKind.SCALAR).map((info) => info.name)
			const uniqueScalars = Array.from(new Set(scalarTypes))
			if (uniqueScalars.includes('GraphQLJSON') || uniqueScalars.includes('JSON')) {
			}
		}

		return imports
	}

	generateCode(): string {
		const imports = this.generateImports()
		const types = this.getAllTypes()
			.filter((info) => info.isGenerated)
			.map((info) => info.code)

		return [...imports, '', ...types].join('\n')
	}

	addDependency(typeName: string, dependency: string): void {
		const typeInfo = this.types.get(typeName)
		if (typeInfo && !typeInfo.dependencies.includes(dependency)) {
			typeInfo.dependencies.push(dependency)
		}
	}

	getDependencyGraph(): Map<string, string[]> {
		const graph = new Map<string, string[]>()

		for (const typeInfo of this.getAllTypes()) {
			graph.set(typeInfo.name, [...typeInfo.dependencies])
		}

		return graph
	}
}
