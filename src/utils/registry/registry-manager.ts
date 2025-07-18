import { ComposerType } from '@types'
import { SchemaComposer } from 'graphql-compose'

export interface TypeRegisterable<T extends ComposerType = ComposerType> {
	hasType(name: string): boolean

	registerType(name: string): void

	getRegisteredTypes(): string[]

	getType(name: string): T | undefined
}

export class RegistryManager<T extends ComposerType = ComposerType> implements TypeRegisterable<T> {
	protected readonly registeredItems: Set<string> = new Set()
	protected readonly schemaComposer: SchemaComposer<unknown>

	constructor(schemaComposer: SchemaComposer<unknown>) {
		this.schemaComposer = schemaComposer
	}

	hasType(name: string): boolean {
		return this.registeredItems.has(name) || this.schemaComposer.has(name)
	}

	registerType(name: string): void {
		this.registeredItems.add(name)
	}

	getRegisteredTypes(): string[] {
		return Array.from(this.registeredItems)
	}

	getType(name: string): T | undefined {
		if (!this.schemaComposer.has(name)) {
			return undefined
		}

		const composer = this.schemaComposer.get(name)
		return composer as T
	}
}
